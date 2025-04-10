#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/pretty_print.hpp"
#include "opium/exceptions.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <iterator>
#include <ranges>


namespace opi {


using query_result =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;


template <std::output_iterator<value> Output>
struct scheme_emitter {
  scheme_emitter(scheme_emitter_context<Output> &ctx, query_result &query);

  template <std::output_iterator<value> ExprOutput>
  void
  emit(value expr, ExprOutput exproutput);

  protected:
  value
  _find_code_type(value code) const;

  private:
  const value m_dont_emit_symbol;
  const query_result &m_query_result;
  scheme_code_transformer m_transformer;
  scheme_emitter_context<Output> &m_ctx;
}; // class opi::scheme_emitter

template <std::output_iterator<value> Output>
template <std::output_iterator<value> ExprOutput>
void
opi::scheme_emitter<Output>::emit(value expr, ExprOutput exproutput)
{
  const value result = m_transformer(expr);
  if (not is(result, m_dont_emit_symbol))
    *exproutput++ = result;
}


// TODO: remove first return, keep only the Scheme code itself
template <std::output_iterator<value> Output>
std::pair<query_result, value>
emite_scheme(scheme_emitter_context<Output> ctx, value plcode, value ppcode);


template <std::output_iterator<value> Output>
value
_emit_specialized_function_body(scheme_emitter_context<Output> &ctx,
                                const predicate &pred, value ppbody,
                                value instantiation)
{
  // Formulate Prolog code to be run for the body
  const value predsignature = pred.argument(0);
  const value clossignature = car(predsignature);
  const value bindclos = list("=", cdr(instantiation), cdr(clossignature));
  const value plcode = list("and", bindclos, pred.body());
  debug("emitting specialized function body for {}", instantiation);
  return emite_scheme(ctx, plcode, ppbody).second;
}


template <std::output_iterator<value> Output>
value
_instantiate_function_template(scheme_emitter_context<Output> &ctx,
  value instantiation, const predicate &pred, value ppcode)
{
  assert(instantiation->t == tag::pair);

  const value signature = car(cdr(ppcode));
  const value paramlist = cdr(signature);
  const value overloadname = car(instantiation);

  static size_t counter = 0; // FIXME
  const value specialident = sym(std::format("{}_{}", overloadname, counter++));
  const value specialsignature = cons(specialident, paramlist);

  // Save specialization with forward declaration
  ctx.specializations.emplace(instantiation, specialident);
  const value specialbody =
      _emit_specialized_function_body(ctx, pred, cdr(cdr(ppcode)), instantiation);

  const value define = list("define", specialsignature, dot, specialbody);
  copy_location(ppcode, define);
  *ctx.global_output++ = define;

  return specialident;
}


template <std::output_iterator<value> Output>
value
instantiate(scheme_emitter_context<Output> &ctx, value type, value x)
{
  // Primitives
  if (ctx.legal_types.contains(type))
    return x;

  // FIXME
  if (match(list("cons-list"), cons("cons-list", "_"))(type))
    return x;

  // Check if suitable specialization already exists
  for (const auto &[instancetype, instance] : ctx.specializations)
  {
    if (type == instancetype)
      return instance;
  }

  // Find matching function template
  stl::vector<template_info> templates;
  std::ranges::copy(ctx.templates | std::views::filter([&](const auto &x) {
                      return std::get<0>(x)(type);
                    }),
                    std::back_inserter(templates));
  // There must be only one match
  assert(templates.size() <= 1);

  if (templates.size() == 1)
  { // Generate new specialization
    const auto &[_, pred, ppcode] = templates.front();
    return _instantiate_function_template(ctx, type, pred, ppcode);
  }

  throw code_transformation_error {
      std::format("Don't know how to instantiate type {}", type), x};
}


template <std::output_iterator<value> Output>
std::pair<query_result, value>
emite_scheme(scheme_emitter_context<Output> ctx, value plcode, value ppcode)
{
  predicate_runtime prt;

  // Tracing of non-terminal variables
  opi::stl::unordered_set<cell *> maybe_nonterminal;
  auto trace_nonterminals = [&](predicate_runtime &, cell *c) {
    maybe_nonterminal.insert(c);
  };

  // Saving query results
  // Helper functions the check if cell corresponds to non-terminal variable
  auto isnonterminal = [&maybe_nonterminal](cell *c) {
    return std::any_of(maybe_nonterminal.begin(), maybe_nonterminal.end(),
                       [c](cell *x) { return find(c) == find(x); });
  };
  query_result results;
  bool success = false;
  auto save_results = [&]() {
    success = true;
    for (const value varname : prt.variables())
    {
      // Reconstruct variable value
      const value cval = reconstruct(prt[varname], [&](cell *c) {
        // Store different markers for unbound terminal and non-terminal vars
        if (isnonterminal(c))
          return "<nonterminal>";
        else
          return "<any>";
      });
      results[varname].insert(cval);
    }
  };

  // Run the query
  const value cellularized = insert_cells(prt, plcode);
  ctx.pl.make_true(prt, cellularized, save_results, trace_nonterminals);

  if (not success)
    throw bad_code {"Prolog query failed", plcode};

  // Emit proper (standard) scheme code
  stl::deque<value> tape;
  scheme_emitter emitter {ctx, results};
  for (const value expr : range(ppcode))
    emitter.emit(expr, std::back_inserter(tape));

  return std::make_pair(results, list(tape));
}


template <std::output_iterator<value> Output>
scheme_emitter<Output>::scheme_emitter(scheme_emitter_context<Output> &ctx,
                                       query_result &query)
: m_dont_emit_symbol {"<dont-emit>"}, m_query_result {query}, m_ctx {ctx}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 template
  const value temppat = list("template", cons("ident", "parms"), dot, "body");
  m_transformer.prepend_rule({list("template"), temppat}, [this](const auto &ms, value fm) {
    const value templatename = ms.at("ident");
    const value params = ms.at("parms");
    const value body = ms.at("body");

    const value ident = m_ctx.prolog_emitter.find_overload_name(templatename);
    const predicate pred = find_predicate_for_template(m_ctx, ident);
    const value code = list("define", cons(ident, params), dot, body);
    copy_location(fm, code);
    const match tempmatch {list(ident), cons(ident, "_")};
    m_ctx.templates.emplace_back(tempmatch, pred, code);

    return m_dont_emit_symbol;
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                       define (function syntax)
  const match defmatch {list("define"),
                        list("define", cons("ident", "parms"), dot, "body")};
  m_transformer.prepend_rule(defmatch, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value parms = ms.at("parms");
    const value body = ms.at("body");

    const value templatename = m_ctx.prolog_emitter.find_overload_name(ident);
    const value type = _find_code_type(ident); // Get template instantiation
    const predicate pred = find_predicate_for_template(m_ctx, templatename);

    // Declaration of the new type
    // NOTE: has to be a forward-declaration to allow for self-referencing
    // NOTE: template instantiation should be forwarded by instantiator
    register_forwarding_type(m_ctx, type);

    // Generate lambda body
    const value specialbody =
        _emit_specialized_function_body(m_ctx, pred, body, type);

    // Return normal Scheme define expression but with specialized body
    return list("define", list(ident, parms), specialbody);
  });


  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "parms", dot, "body")};
  m_transformer.prepend_rule(lambdamatch, [this](const auto &ms, value fm) {
    const value parms = ms.at("parms");
    const value body = ms.at("body");

    const value ident = m_ctx.prolog_emitter.find_overload_name(fm);
    const value type = _find_code_type(fm); // Get template instantiation
    const predicate pred = find_predicate_for_template(m_ctx, ident);

    // Declaration of the new type
    // NOTE: it is a forward-declaration just in case the lambda is self-referencing
    // NOTE: lambda instantiation should be forwarded by instantiator
    register_forwarding_type(m_ctx, type);

    // Generate lambda body
    const value specialbody =
        _emit_specialized_function_body(m_ctx, pred, body, type);

    // Return normal Scheme lambda expression but with specialized body
    return list("lambda", parms, specialbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  m_transformer.append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const value form = cons(f, xs);
    return list(range(form) | std::views::transform(std::ref(m_transformer)));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atom
  m_transformer.append_rule({nil, "x"}, [this](const auto &ms) {
    const value x = ms.at("x");

    // Get type of this atom
    value type = nil;
    if (not m_ctx.prolog_emitter.find_code_type(x, type))
      throw code_transformation_error {
          std::format("Don't know what type to use for {}", x), x};

    // Get deduced type if it is not given explicitly
    const auto it = m_query_result.find(type);
    if (it != m_query_result.end())
    {
      const auto &possibilities = it->second;
      if (possibilities.size() != 1)
        throw code_transformation_error {
            std::format("Ambiguous type for {}: {}", x, list(possibilities)), x};
      type = *possibilities.begin();
    }

    // instantiate
    return instantiate(m_ctx, type, x);
  });
}


template <std::output_iterator<value> Output>
value
opi::scheme_emitter<Output>::_find_code_type(value code) const
{
  const value type = m_ctx.prolog_emitter.find_code_type(code);
  const auto it = m_query_result.find(type);
  if (it != m_query_result.end())
  {
    const auto &possibilities = it->second;
    if (possibilities.size() != 1)
      throw code_transformation_error {
          std::format("Ambiguous type: {}", list(possibilities)), code};
    return *possibilities.begin();
  }
  else
    return type;
}


static std::pair<query_result, value>
translate_to_scheme(size_t &counter, prolog &pl, value code)
{
  const prolog_formatter plfmt;
  pretty_printer pprint_pl {plfmt};

  const scheme_formatter scmfmt;
  pretty_printer pprint_scm {scmfmt};

  // Compose translator from Scheme to Prolog
  symbol_generator genuid {counter, "uid{}"};
  scheme_code_flattener flatten {genuid};
  scheme_unique_identifiers insert_uids {genuid};
  scheme_to_prolog to_prolog {counter};
  prolog_cleaner pl_cleaner;

  // TODO: find a better way
  int warned = false;
  for (const auto &[predicatename, predicate] : pl.predicates())
  {
    if (predicatename == "result-of")
    {
      const value signature = predicate.argument(0);
      if (signature->t == tag::pair and issym(car(signature)))
      {
        const value identifier = car(signature);

        if (not warned++)
          warning("extracting forward-types from support predicates");
        to_prolog.add_global(identifier, identifier);
      }
    }
  }

  // Translate the input expression
  const value ppcode = list(range(code)
                     | std::views::transform(std::ref(flatten))
                     | std::views::transform(std::ref(insert_uids)));
  const value plcode = list(range(to_prolog.transform_block(ppcode)) |
                            std::views::transform(std::ref(pl_cleaner)));

  debug("type-check Prolog code:\n{}", pprint_pl(plcode));

  const value unresolved = list(to_prolog.unresolved());
  if (length(unresolved) > 0)
  {
    for (const value symbol : range(unresolved))
    {
      error("unresolved symbol: \e[1m‘{}’\e[0m", symbol);
      source_location location;
      if (get_location(symbol, location))
        std::cerr << display_location(location, 0, "\e[38;5;1;1m") << std::endl;
    }
    throw bad_code {"unresolved symbols", unresolved};
  }

  // Collect predicates generated during translation
  for (const predicate &pred : to_prolog.predicates())
  {
    if (loglevel >= loglevel::info)
    {
      debug("add generated predicate:\n{}{} :-\n  {}", pred.name(),
            list(pred.arguments()), pprint_pl(pl_cleaner(pred.body()), 2));
    }
    pl.add_predicate(pred);
  }

  // Translate the code to proper scheme
  std::deque<value> main_tape;
  scheme_emitter_context ctx {pl, to_prolog, std::back_inserter(main_tape)};
  auto [result, main] = emite_scheme(ctx, plcode, ppcode);

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {result, resultcode};
}


} // namespace opi

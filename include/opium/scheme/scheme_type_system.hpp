#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/pretty_print.hpp"

#include <ranges>


namespace opi {


using query_result =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;

using template_info = std::tuple<match, predicate, value>;

template <std::output_iterator<value> Output>
struct scheme_emitter_context {
  scheme_emitter_context(const prolog &pl, const scheme_to_prolog &pl_emitter,
                         Output output)
  : global_output {output}, pl {pl}, prolog_emitter {pl_emitter}
  { }

  Output global_output; /**< Output tape for supplementary code */

  opi::stl::deque<template_info> templates; /**< Template definitions */
  opi::stl::unordered_map<value, value> specializations; /**< Cache for produced
                                                              template specializations */
  opi::stl::unordered_set<value> legal_types = {
      "num", "nil", "str", "sym", "boolean",

      // FIXME
      "+", "list", "car", "cdr", "values", "pair?"};

  const prolog &pl; /**< Storage for predicates */
  const scheme_to_prolog &prolog_emitter; /**< Storage for type info */
}; // struct opi::scheme_emitter_context


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

template <std::output_iterator<value> Output>
std::pair<query_result, value>
_translate(scheme_emitter_context<Output> ctx, value plcode, value ppcode);

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
  return _translate(ctx, plcode, ppbody).second;
}


template <std::output_iterator<value> Output>
value
_instantiate(scheme_emitter_context<Output> &ctx, value type, value x)
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
  {
    assert(type->t == tag::pair);

     // Generate new specialization
    const auto &[_, pred, ppcode] = templates.front();

    const value signature = car(cdr(ppcode));
    const value paramlist = cdr(signature);
    const value overloadname = car(type);

    static size_t counter = 0; // FIXME
    const value specialident = sym(std::format("{}_{}", overloadname, counter++));
    const value specialsignature = cons(specialident, paramlist);

    // Save specialization with forward declaration
    ctx.specializations.emplace(type, specialident);
    const value specialbody =
        _emit_specialized_function_body(ctx, pred, cdr(cdr(ppcode)), type);

    const value define = list("define", specialsignature, dot, specialbody);
    copy_location(ppcode, define);
    *ctx.global_output++ = define;

    return specialident;
  }

  throw code_transformation_error {
      std::format("Don't know how to instantiate type {}", type), x};
}


template <std::output_iterator<value> Output>
std::pair<query_result, value>
_translate(scheme_emitter_context<Output> ctx, value plcode, value ppcode)
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
    throw std::runtime_error {"Prolog query failed"};

  // Emit proper (standard) scheme code
  stl::deque<value> tape;
  scheme_emitter emitter {ctx, results};
  for (const value expr : range(ppcode))
    emitter.emit(expr, std::back_inserter(tape));

  return std::make_pair(results, list(tape));
}


// NOTE: making effort to ensure there is only a single predicate matching
template <std::output_iterator<value> Output>
predicate
_find_predicate_for_template(const scheme_emitter_context<Output> &ctx,
                             value template_name)
{
  // Matcher to match suiting predicate based on predicate signature
  const match m {
      list("result-of", template_name),
      list("result-of", cons(cons(template_name, "_"), "args"), "result")};

  // Find suiting predicate
  const auto f = [&](const auto &pred) { return m(pred.signature()); };
  opi::stl::list<predicate> preds;
  std::ranges::copy(ctx.prolog_emitter.predicates() | std::views::filter(f),
                    std::back_inserter(preds));

  assert(preds.size() == 1);
  return preds.front();
}


template <std::output_iterator<value> Output>
void
_register_forwarding_type(scheme_emitter_context<Output> &ctx, value type)
{
  const bool ok = ctx.legal_types.insert(type).second;
  assert(ok && "Lambda instance type already exists");
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
    const predicate pred = _find_predicate_for_template(m_ctx, ident);
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
    const predicate pred = _find_predicate_for_template(m_ctx, templatename);

    // Declaration of the new type
    // NOTE: has to be a forward-declaration to allow for self-referencing
    // NOTE: template instantiation should be forwarded by instantiator
    _register_forwarding_type(m_ctx, type);

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
    const predicate pred = _find_predicate_for_template(m_ctx, ident);

    // Declaration of the new type
    // NOTE: it is a forward-declaration just in case the lambda is self-referencing
    // NOTE: lambda instantiation should be forwarded by instantiator
    _register_forwarding_type(m_ctx, type);

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
    return _instantiate(m_ctx, type, x);
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
scheme_type_check(size_t gensym_counter, prolog &pl, value code)
{
  const prolog_formatter plfmt;
  pretty_printer pprint_pl {plfmt};

  const scheme_formatter scmfmt;
  pretty_printer pprint_scm {scmfmt};

  // Compose translator from Scheme to Prolog
  symbol_generator genuid {gensym_counter};
  scheme_code_flattener flatten {genuid};
  scheme_unique_identifiers insert_uids {genuid};
  scheme_to_prolog to_prolog {gensym_counter};
  prolog_cleaner pl_cleaner;

  // FIXME
  to_prolog.add_global("pair?", "pair?");
  to_prolog.add_global("cons", "cons");
  to_prolog.add_global("list", "list");
  to_prolog.add_global("-", "-");
  to_prolog.add_global("+", "+");
  to_prolog.add_global(">", ">");
  to_prolog.add_global("some?", "some?");
  to_prolog.add_global("real_time", "real_time");
  to_prolog.add_global("poll", "poll");
  to_prolog.add_global("values", "values");
  to_prolog.add_global("car", "car");
  to_prolog.add_global("cdr", "cdr");
  to_prolog.add_global("<thread>", "<thread>");

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
    throw std::runtime_error {"unresolved symbols"};
  }

  // Collect predicates generated during translation
  for (const predicate &pred : to_prolog.predicates())
  {
    if (loglevel >= loglevel::info)
    {
      info("add generated predicate:");
      std::cerr << std::format("{}{} :-\n  ", pred.name(),
                               list(pred.arguments()));
      pprint_pl(std::cout, pl_cleaner(pred.body()), 2);
      std::cerr << std::endl;
    }
    pl.add_predicate(pred);
  }

  // Translate the code to proper scheme
  std::deque<value> main_tape;
  scheme_emitter_context ctx {pl, to_prolog, std::back_inserter(main_tape)};
  auto [result, main] = _translate(ctx, plcode, ppcode);

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {result, resultcode};
}


} // namespace opi
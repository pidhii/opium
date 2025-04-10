#pragma once

#include "opium/scheme/scheme_type_system.hpp"


namespace opi {

template <std::output_iterator<value> Output>
std::tuple<query_result, value, scheme_type_location_map>
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
  scheme_emitter<Output> emitter {ctx, results};
  for (const value expr : range(ppcode))
    emitter.emit(expr, std::back_inserter(tape));

  // Build the type location map
  scheme_type_location_map type_map =
      build_type_location_map(ctx.prolog_emitter, ppcode);
  type_map.substitute_type_aliases(results);

  return {results, list(tape), type_map};
}


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
  return std::get<1>(emite_scheme(ctx, plcode, ppbody));
}


/**
 * Generate a distinctive name for a template instance using parameter types for
 * unique identification
 *
 * This function creates a string representation of a template instance in the format
 * `identifier<param1,param2,...>` with recursive handling of parameter types.
 * 
 * \param type The template instance type value
 * \param os The output stream to write the formatted name to
 */
// TODO: move to cpp file
inline void
pretty_template_instance_name(value type, std::ostream &os)
{
  if (type->t != tag::pair)
  {
    // Not a template instance, return as is
    os << type;
    return;
  }

  value identifier = car(type);
  const value params = cdr(type);

  assert(issym(identifier));

  // Remove syntax-related prefix inserted by Prolog emitter
  if (sym_name(identifier).starts_with("template:"))
    identifier = sym(sym_name(identifier).substr(sizeof("template:") - 1));

  if (nil == params)
  {
    // No parameters, just return the identifier
    os << identifier;
    return;
  }

  // Write the identifier and opening bracket
  os << identifier << "<";

  // Add parameters with recursive handling
  for (std::string prefix = ""; const value param : range(params))
  {
    os << prefix;
    prefix = ",";

    // Recursively format parameter if it's a template instance
    pretty_template_instance_name(param, os);
  }

  os << ">";
}


template <std::output_iterator<value> Output>
value
_instantiate_function_template(scheme_emitter_context<Output> &ctx,
  value instantiation, const predicate &pred, value ppcode)
{
  assert(instantiation->t == tag::pair);

  const value signature = car(cdr(ppcode));
  const value paramlist = cdr(signature);

  std::ostringstream identbuf;
  pretty_template_instance_name(instantiation, identbuf);
  const value specialident = sym(identbuf.str());
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


} // namespace opi

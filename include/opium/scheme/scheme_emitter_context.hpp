#pragma once

#include "opium/scheme/scheme_transformations.hpp"
#include "opium/match.hpp"
#include "opium/prolog.hpp"


namespace opi {

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
      "+", "list", "car", "cdr", "values", "pair?", "string-append"};

  const prolog &pl; /**< Storage for predicates */
  const scheme_to_prolog &prolog_emitter; /**< Storage for type info */
}; // struct opi::scheme_emitter_context


/** Implementation for a function below */
predicate
_find_predicate_for_template(const scheme_to_prolog &prolog_emitter,
                             value template_name);

/**
 * Find predicate object associated to the template
 *
 * \param ctx Emitter context
 * \param template_name Name of the function template
 * \return Prolog predicate
 */
template <std::output_iterator<value> Output>
predicate
find_predicate_for_template(const scheme_emitter_context<Output> &ctx,
                             value template_name)
{ return _find_predicate_for_template(ctx.prolog_emitter, template_name); }


/**
 * Enable forwarding of the values for the given type by instantiator
 *
 * \param ctx Emitter context
 * \param type Type object
 * \see instantiate()
 *
 */
template <std::output_iterator<value> Output>
void
register_forwarding_type(scheme_emitter_context<Output> &ctx, value type)
{
  const bool ok = ctx.legal_types.insert(type).second;
  assert(ok && "Lambda instance type already exists");
}


} // namespace opi
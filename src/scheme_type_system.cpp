/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "opium/scheme/scheme_type_system.hpp"
#include "opium/logging.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_emitter.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"


opi::value
opi::generate_function_template_body(scheme_emitter_context &ctx,
                                     value instantiation, value type_template,
                                     value ppbody)
{

  assert(instantiation->t == tag::pair);
  assert(issym(car(instantiation), "#dynamic-function-dispatch"));

  predicate_runtime prt;
  execution_timer t1 {"match function instantiation on template"};
  if (not match_arguments(prt, insert_cells(prt, type_template), instantiation, true))
  {
    error("failed to match instantion on template");
    error("template:       {}",
          car(cdr(reconstruct(type_template, stringify_unbound_variables))));
    error("specialization: {}",
          car(cdr(reconstruct(instantiation, stringify_unbound_variables))));
    throw bad_code {"Template specialization failure"};
  }
  t1.stop();

  query_result results;
  execution_timer t2 {"reconstruct function instantiation types"};
  for (const value varname : prt.variables())
    results[varname].emplace(reconstruct(prt[varname]));
  t2.stop();

  stl::deque<value> tape;
  scheme_emitter emitter {ctx, results};
  for (const value expr : range(ppbody))
    emitter.emit(expr, std::back_inserter(tape));

  return list(tape);
}


opi::value
opi::instantiate_function_template(scheme_emitter_context &ctx, value type)
{
  if (type->t != tag::pair)
  {
    // TODO/FIXME: typically happens during negative assetions, so don't print
    // error unless the type is <any>
    if (type != "<any>")
    {
      error("Can't create template function specialization.\n"
            "Expected #dynamic-function-dispatch structure, got {}", type);
      source_location location;
      if (get_location(type, location))
        error("{}", display_location(location));
    }
    throw bad_code {"Can't create template function specialization.", type};
  }
  assert(type->t == tag::pair);
  assert(issym(car(type), "#dynamic-function-dispatch"));

  const value tag = car(cdr(type));
  const function_template &functemplate = ctx.find_template(tag);
  const value typetemplate = functemplate.typetemplate;
  const value ppdefinition = functemplate.ppdefinition;

  value specialident = nil;
  if (functemplate.context.find_function_template_speciailization(type,
                                                                  specialident))
    return specialident;

  // Generate identifier for the specialization we are about to create
  std::ostringstream identbuf;
  pretty_template_instance_name(type, identbuf);
  specialident = sym(identbuf.str());
  const value ppsignature = car(cdr(ppdefinition));

  // Save specialization with forward declaration
  functemplate.context.register_function_template_specialization(tag, type,
                                                                 specialident);

  // Generate specialized body
  const value specialbody = generate_function_template_body(
      ctx, type, typetemplate, cdr(cdr(ppdefinition)));

  // Create define-statement for the specialization
  const value specialsignature = cons(specialident, cdr(ppsignature));
  const value define = list("define", specialsignature, dot, specialbody);
  copy_location(ppdefinition, define);
  *functemplate.context.output()++ = define;

  return specialident;
}


std::pair<opi::value, opi::scheme_type_location_map>
opi::emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode)
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
        // FIXME: breaks matching during template specialization
        if (isnonterminal(c))
          return "<nonterminal>";
        else
          return "<any>";
      });
      results[varname].insert(cval);
    }
  };

  // Run the query
  execution_timer query_timer {"Prolog query"};
  const value cellularized = insert_cells(prt, plcode);
  ctx.pl().make_true(prt, cellularized, save_results, trace_nonterminals);
  query_timer.stop();
  query_timer.report();

  if (not success)
  {
    if (global_flags.contains("IgnoreFailedQuery"))
      return {nil, {}};
    else
      throw bad_code {"Prolog query failed (use -fIgnoreFailedQuery to ignore)", plcode};
  }
  else
    debug("\e[1mQUERY SUCCEEDED\e[0m");

  // Emit proper (standard) scheme code
  stl::deque<value> tape;
  scheme_emitter emitter {ctx, results};
  for (const value expr : range(ppcode))
    emitter.emit(expr, std::back_inserter(tape));

  // Build the type location map
  scheme_type_location_map type_map =
      build_type_location_map(ctx.prolog_emitter(), ppcode);
  type_map.substitute_type_aliases(results);

  return {list(tape), type_map};
}


void
opi::pretty_template_instance_name(value type, std::ostream &os)
{
  if (type == nil)
  {
    os << "<>";
    return;
  }

  if (type->t != tag::pair)
  {
    // Not a template instance, return as is
    os << type;
    return;
  }

  if (issym(car(type), "#dynamic-function-dispatch"))
  {
    const value tag = car(cdr(type));
    const value typeparams = car(cdr(cdr((type))));
    const value resulttypes = car(cdr(cdr(cdr(type))));
    os << tag << "<";
    for (std::string prefix = ""; const value type : range(typeparams))
    {
      os << prefix;
      prefix = "_";
      pretty_template_instance_name(type, os);
    }
    os << "_";
    pretty_template_instance_name(resulttypes, os);
    os << ">";
    return;
  }

  if (not issym(car(type)))
  {
    os << "#<";
    for (std::string prefix = ""; const value x : range(car(type)))
    {
      os << prefix;
      prefix = "_";
      pretty_template_instance_name(x, os);
    }
    os << ">#";
    return;
  }

  os << car(type) << "<";
  for (std::string prefix = ""; const value x : range(cdr(type)))
  {
    os << prefix;
    prefix = "_";
    pretty_template_instance_name(x, os);
  }
  os << ">";
}

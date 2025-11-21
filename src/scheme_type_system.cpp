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
#include "opium/scheme/translator/exceptions.hpp"
#include "opium/scheme/translator/scheme_emitter.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"


opi::value
opi::generate_function_template_body(scheme_emitter_context &ctx,
                                     value instantiation, value type_template,
                                     value ppbody)
{
  assert(ispair(instantiation));
  assert(issym(car(instantiation), "#dynamic-function-dispatch"));

  predicate_runtime prt;
  OPI_BLOCK_BENCHMARK("match function instantiation on template", {
    if (not match_arguments(prt, insert_cells(prt, type_template), instantiation))
    {
      error("failed to match instantion on template");
      error("template:       {}",
            car(cdr(reconstruct(type_template, stringify_unbound_variables))));
      error("specialization: {}",
            car(cdr(reconstruct(instantiation, stringify_unbound_variables))));
      throw bad_code {"Template specialization failure"};
    }
  })

  type_bindings results;
  OPI_BLOCK_BENCHMARK("reconstruct function instantiation types", {
    for (const value varname : prt.variables())
      results[varname].emplace(reconstruct(prt[varname], ignore_unbound_variables));
  })

  stl::deque<value> tape;
  scheme_emitter emitter {ctx, results};
  for (const value expr : range(ppbody))
    emitter.emit(expr, std::back_inserter(tape));

  return list(tape);
}


opi::value
opi::instantiate_function_template(scheme_emitter_context &ctx, value type)
{
  assert(ispair(type));
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
  // NOTE: code emmission has to be done in a dedicated local/sub -context
  code_tape auxtape;
  scheme_emitter_context localctx {functemplate.context, auxtape};
  value specialbody = generate_function_template_body(
      localctx, type, typetemplate, cdr(cdr(ppdefinition)));
  // insert all the auxiliary code in the head of the function body
  specialbody = append(list(auxtape), specialbody);

  // Create define-statement for the specialization
  const value specialsignature = cons(specialident, cdr(ppsignature));
  const value define = list("define", specialsignature, dot, specialbody);
  copy_location(ppdefinition, define);
  *functemplate.context.output()++ = define;

  return specialident;
}


std::pair<opi::value, opi::scheme_type_location_map>
opi::emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode,
                 const std::optional<prolog_guide_function> &guide)
{
  predicate_runtime prt;

  // Saving query results
  type_bindings results;
  bool success = false;
  auto save_results = [&]() {
    success = true;
    for (const value varname : prt.variables())
    {
      // Reconstruct variable value
      execution_timer timer {"reconstruct query results"};
      const value cval = reconstruct<clone_unbound_variables_t>(prt[varname]);
      timer.stop();
      results[varname].insert(cval);
    }
  };

  // Run the query
  execution_timer query_timer {"Prolog query"};
  const value cellularized = insert_cells(prt, plcode);
  { // disable implicit location propagation for better performance
    opi::utl::state_saver _ {g_propagate_locations_on_cons};
    g_propagate_locations_on_cons = false;
    if (guide.has_value())
      ctx.pl().make_true(cellularized, save_results, *guide);
    else
      ctx.pl().make_true(cellularized, save_results);
  }
  query_timer.stop();
  query_timer.report();

  if (not success)
  {
    if (global_flags.contains("IgnoreFailedQuery"))
      return {nil, {}};
    else
      throw typecheck_failure {
          "Prolog query failed (use -fIgnoreFailedQuery to ignore)"};
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
      build_type_location_map(ctx.ctm(), ppcode);
  type_map.substitute_type_aliases(results);

  return {list(tape), type_map};
}


void
opi::pretty_template_instance_name(value type, std::ostream &os)
{
  if (type == nil)
  {
    os << "nil";
    return;
  }

  if (not ispair(type))
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

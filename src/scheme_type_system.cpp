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

  const value paramtypes = car(cdr(cdr(instantiation)));
  const value resulttype = car(cdr(cdr(cdr(instantiation))));

  const value paramsymbols = car(cdr(cdr(type_template)));
  const value resultsymbol = car(cdr(cdr(cdr(type_template))));
  const value plbody       = car(cdr(cdr(cdr(cdr(type_template)))));

  const value parambind = list("=", paramsymbols, paramtypes);
  const value resultbind = list("=", resultsymbol, resulttype);
  const value expr = list("and", parambind, resultbind, plbody);

  debug("generating specialized body:\n"
        "\tinstantiation = {}\n"
        "\ttemplate      = {}\n"
        "\tresult expr   = {}",
      instantiation, type_template, pprint_pl(expr));

  // Emit body within a nested context
  code_tape localtape;
  scheme_emitter_context localctx {ctx, localtape};
  value specialbody = emit_scheme(localctx, expr, ppbody).first;

  // Merge any accompanying code into the body
  specialbody = append(list(localtape), specialbody);

  return specialbody;
}


opi::value
opi::instantiate_function_template(scheme_emitter_context &ctx, value type)
{
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

  if (not success)
    throw bad_code {"Prolog query failed", plcode};
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
    const value resulttype = car(cdr(cdr(cdr(type))));
    os << tag << "<";
    for (std::string prefix = ""; const value type : range(typeparams))
    {
      os << prefix;
      prefix = "_";
      pretty_template_instance_name(type, os);
    }
    os << ">->";
    pretty_template_instance_name(resulttype, os);
    return;
  }

  for (std::string prefix = ""; const value x : range(type))
  {
    os << prefix;
    prefix = "_";
    pretty_template_instance_name(x, os);
  }
}

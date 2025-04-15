#pragma once

#include "opium/scheme/scheme_type_system.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/source_location.hpp"


namespace opi {

// TODO: move to cpp file
inline value
_instantiate_function_template(scheme_emitter_context &ctx, value instantiation,
                               value typetemplate, value ppdefinition,
                               scheme_emitter_context &template_ctx)
{
  assert(instantiation->t == tag::pair);
  assert(issym(car(instantiation), "#dynamic-function-dispatch"));

  const value template_tag = car(cdr(instantiation));
  const value paramtypes = car(cdr(cdr(instantiation)));
  const value resulttype = car(cdr(cdr(cdr(instantiation))));

  const value paramsymbols = car(cdr(cdr(typetemplate)));
  const value resultsymbol = car(cdr(cdr(cdr(typetemplate))));
  const value plbody       = car(cdr(cdr(cdr(cdr(typetemplate)))));

  value specialident = nil;
  if (template_ctx.find_function_template_speciailization(instantiation,
                                                          specialident))
    return specialident;

  const value ppsignature = car(cdr(ppdefinition));
  const value ppbody = cdr(cdr(ppdefinition));

  const value parambind = list("=", paramsymbols, paramtypes);
  const value resultbind = list("=", resultsymbol, resulttype);
  const value expr = list("and", parambind, resultbind, plbody);

  std::ostringstream identbuf;
  pretty_template_instance_name(instantiation, identbuf);
  specialident = sym(identbuf.str());
  const value specialsignature = cons(specialident, cdr(ppsignature));

  // Save specialization with forward declaration
  template_ctx.register_function_template_specialization(
      template_tag, instantiation, specialident);

  // Emit body within a nested context
  code_tape localtape;
  scheme_emitter_context localctx {ctx, localtape};
  value specialbody = emit_scheme(localctx, expr, ppbody).first;

  // Merge any accompanying code into the body
  specialbody = append(list(localtape), specialbody);
  
  const value define = list("define", specialsignature, dot, specialbody);
  copy_location(ppdefinition, define);
  *template_ctx.output++ = define;

  return specialident;
}

// TODO: move to cpp file
inline value
instantiate(scheme_emitter_context &ctx, value type, value x)
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

  // Handle function tempalte instantiations
  if (type->t == tag::pair and issym(car(type), "#dynamic-function-dispatch"))
  {
    debug("instantiating {} from {}", x, type);
    assert(type->t == tag::pair);
    assert(issym(car(type), "#dynamic-function-dispatch"));

    const value tag = car(cdr(type));
    const function_template &functemplate = ctx.find_template(tag);
    const value typetemplate = functemplate.typetemplate;
    const value ppdefinition = functemplate.ppdefinition;
    return _instantiate_function_template(ctx, type, typetemplate, ppdefinition,
                                          functemplate.context);
  }

  throw code_transformation_error {
      std::format("Don't know how to instantiate type {}", type), x};
}

// TODO: move to cpp file
inline std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode)
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

  // Build the type location map
  scheme_type_location_map type_map =
      build_type_location_map(ctx.prolog_emitter, ppcode);
  type_map.substitute_type_aliases(results);

  return {list(tape), type_map};
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
// TODO: come up with better format
inline void
pretty_template_instance_name(value type, std::ostream &os)
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
      prefix = ",";
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


} // namespace opi

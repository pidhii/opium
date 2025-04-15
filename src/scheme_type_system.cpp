#include "opium/scheme/scheme_type_system.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_emitter.hpp"
#include "opium/source_location.hpp"


/**
 * Generate implementation of a function template specialization
 *
 * \param ctx Current emitter-context
 * \param instantiation Function template instantiation from the TypeChecker
 * \param typetemplate Function template from the TypeChecker
 * \param ppdefinition Actual template-form from the (preprocessed) input code
 * \param template_ctx Native context of the function template
 */
static opi::value
_instantiate_function_template_impl(opi::scheme_emitter_context &ctx,
                                    opi::value instantiation,
                                    opi::value type_template,
                                    opi::value ppdefinition,
                                    opi::scheme_emitter_context &template_ctx)
{
  using namespace opi;

  assert(instantiation->t == tag::pair);
  assert(issym(car(instantiation), "#dynamic-function-dispatch"));

  const value template_tag = car(cdr(instantiation));
  const value paramtypes = car(cdr(cdr(instantiation)));
  const value resulttype = car(cdr(cdr(cdr(instantiation))));

  const value paramsymbols = car(cdr(cdr(type_template)));
  const value resultsymbol = car(cdr(cdr(cdr(type_template))));
  const value plbody       = car(cdr(cdr(cdr(cdr(type_template)))));

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
  *template_ctx.output()++ = define;

  return specialident;
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
  return _instantiate_function_template_impl(
      ctx, type, typetemplate, ppdefinition, functemplate.context);
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
  const value cellularized = insert_cells(prt, plcode);
  ctx.pl().make_true(prt, cellularized, save_results, trace_nonterminals);

  if (not success)
    throw bad_code {"Prolog query failed", plcode};

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

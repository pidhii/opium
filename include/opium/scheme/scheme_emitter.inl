#pragma once

#include "opium/scheme/scheme_type_system.hpp"


namespace opi {

template <std::output_iterator<value> Output>
template <std::output_iterator<value> ExprOutput>
void
scheme_emitter<Output>::emit(value expr, ExprOutput exproutput)
{
  const value result = m_transformer(expr);
  if (not is(result, m_dont_emit_symbol))
    *exproutput++ = result;
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
scheme_emitter<Output>::_find_code_type(value code) const
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

} // namespace opi

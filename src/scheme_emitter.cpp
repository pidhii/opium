#include "opium/scheme/scheme_emitter.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/value.hpp"
#include <asm-generic/errno.h>


opi::scheme_emitter::scheme_emitter(scheme_emitter_context &ctx, query_result &query)
: m_dont_emit_symbol {"<dont-emit>"}, m_query_result {query}, m_ctx {ctx}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 template
  const value temppat = list("template", cons("ident", "parms"), dot, "body");
  const auto temprule = [this](const auto &ms, value fm) {
    const value identifier = ms.at("ident");

    // Register this identifier as a reference to a function template 
    // NOTE: concrete overload referenced by the identifier is subject to be
    // be determined by the TypeCheck; this registration needed just to remember
    // to substitute this particular identifier with another identifier refering
    // to an apropriate specialization
    m_ctx.register_identifier_for_function_template(identifier);

    // Register all the pieces of information need for generation of function
    // template specializations in future
    const value typetemplate = _find_code_type(fm);
    m_ctx.register_template(m_ctx.prolog_emitter().find_code_tag(fm),
                            {fm, typetemplate, m_ctx});

    return m_dont_emit_symbol;
  };
  m_transformer.prepend_rule({list("template"), temppat}, temprule);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "parms", dot, "body")};
  m_transformer.prepend_rule(lambdamatch, [this](const auto &ms, value fm) {
    const value parms = ms.at("parms");
    const value body = ms.at("body");

    const value type_template = _find_code_type(fm);
    const value type = _find_code_type(car(fm));
    warning("lambda type_template: {}", type_template);
    warning("lambda type: {}", type);

    const value specialbody = generate_function_template_body(
        m_ctx, type, type_template, body);

    // Return normal Scheme lambda expression but with specialized body
    return list("lambda", parms, dot, specialbody);
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
    if (not m_ctx.prolog_emitter().find_code_type(x, type))
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
    if (m_ctx.identifier_refers_to_function_template(x))
      return instantiate_function_template(m_ctx, type);
    else
      return x;

    return x;
  });
}

opi::value
opi::scheme_emitter::_find_code_type(value code) const
{
  const value type = m_ctx.prolog_emitter().find_code_type(code);
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

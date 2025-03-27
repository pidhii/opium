#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"
#include <readline/readline.h>


opi::scheme_code_flattener::scheme_code_flattener(symbol_generator &gensym)
: m_gensym {gensym}
{
  // function invocation
  // -------------------
  // Pull output nested comound expressions from inside the invocation by
  // separating them into bindings to temporary variables to replace
  // corresponding expressions in the invocation form:
  //
  // (<expr1> <expr2> ...) ->
  // (let ((<tmp1> T[<expr1>])
  //       (<tmp2> T[<expr2>])
  //             ...          )
  //   (<tmp1> <tmp2> ...))
  //
  append_rule(match {nil, list("form", "...")}, [this](const auto &ms) {
    value binds = nil;
    value result = nil;
    for (const value x : range(ms.at("form")))
    {
      if (x->t == tag::pair)
      {
        const value uid = m_gensym();
        binds = append(binds, list(list(uid, (*this)(x))));
        result = append(result, list(uid));
      }
      else
        result = append(result, list(x));
    }
    // Don't bloat output with empty let-statements
    if (binds == nil)
      return result;
    else
      return list("let", binds, result);
  });

  // atoms
  append_rule(match {nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}

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
  append_rule({nil, list("form", "...")}, [this](const auto &ms, value fm) {
    value binds = nil;
    value result = nil;
    for (const value x : range(ms.at("form")))
    {
      if (x->t == tag::pair)
      {
        const value uid = m_gensym();
        copy_location(x, uid);
        binds = append(binds, list(list(uid, (*this)(x))));
        result = append(result, list(uid));
      }
      else
        result = append(result, list(x));
    }

    // Don't bloat output with empty let-statements
    if (binds == nil)
    {
      copy_location(fm, result);
      return result;
    }
    else
      return list("let", binds, result);
  });

  // atoms
  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}

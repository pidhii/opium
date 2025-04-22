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


#pragma once

#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <iterator>


namespace opi {


using query_result =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;


struct scheme_emitter {
  scheme_emitter(scheme_emitter_context &ctx, query_result &query);

  template <std::output_iterator<value> ExprOutput>
  void
  emit(value expr, ExprOutput exproutput)
  {
    const value result = m_transformer(expr);
    if (not is(result, m_dont_emit_symbol))
      *exproutput++ = result;
  }

  value
  transform_list(value l)
  {
    opi::stl::vector<value> result;
    for (const value expr : range(l))
      emit(expr, std::back_inserter(result));
    return list(result);
  }

  value
  transform_inner_block_into_expression(value block);

  protected:
  value
  _find_code_type(value code) const;

  private:
  const value m_dont_emit_symbol;
  const query_result &m_query_result;
  ext_scheme_code_transformer m_transformer;
  scheme_emitter_context &m_ctx;
}; // class opi::scheme_emitter


} // namespace opi

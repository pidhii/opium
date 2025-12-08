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

#include "opium/code_transformer.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/value.hpp"


namespace opi {

using type_bindings =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;

struct scheme_emitter: ext_scheme_code_transformer {
  scheme_emitter(scheme_emitter_context &ctx, const type_bindings &query);

  value
  transform_list(value l) const
  { return list(std::views::transform(range(l), std::ref(*this))); }

  value
  transform_block(value l) const override
  {
    stl::vector<value> blockhead, blockbody;
    scheme_emitter_context blockctx {m_ctx, blockhead};
    scheme_emitter blockemitter {blockctx, m_type_bindings};
    for (const value expr : range(l))
    {
      const value texpr = blockemitter(expr);
      if (texpr != nil)
        blockbody.push_back(texpr);
    }
    std::copy(blockbody.begin(), blockbody.end(),
              std::back_inserter(blockhead));
    return list(blockhead);
  }

  value
  transform_inner_block_into_expression(value block);

  protected:
  value
  _find_code_type(value code) const;

  value
  _unfold_types(opi::value x) const;

  value
  _unfold_pattern_type(opi::value pattern) const;

  private:
  const type_bindings &m_type_bindings;
  scheme_emitter_context &m_ctx;
}; // class opi::scheme_emitter


} // namespace opi

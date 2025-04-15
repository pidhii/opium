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

#include "opium/value.hpp"
#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/stl/unordered_map.hpp"
#include "opium/format.hpp" // IWYU pragma: export

#include <concepts>


namespace opi {


template <typename T>
concept value_mapping = requires(T &x, value k)
{
  { x.contains(k) } -> std::convertible_to<bool>;
  { x.at(k) } -> std::convertible_to<value>;
  { x.insert(std::make_pair(k, k)) };
  { x.insert_or_assign(k, k) };
};


class match {
  public:
  match(value literals, value pattern)
  : m_literals {literals}, m_pattern {pattern}
  { }

  value
  pattern() const noexcept
  { return m_pattern; }

  template <value_mapping Mapping>
  bool
  operator () (value expr, Mapping &result) const
  { return _match(m_pattern, expr, result); }

  bool
  operator () (value expr) const
  {
    opi::stl::unordered_map<value, value> _;
    return _match(m_pattern, expr, _);
  }

  private:
  template <value_mapping Mapping>
  bool
  _match(value pat, value expr, Mapping &result) const;

  private:
  value m_literals;
  value m_pattern;
};


} // namespace opi

#include "opium/match.inl"

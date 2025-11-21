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

#include "opium/match.hpp"
#include "opium/value.hpp"
#include "opium/stl/deque.hpp"


namespace opi {

struct match_translation_rules {
  struct rule {
    match ctor_match;
    match type_match;
    value predicate;
    value unpack;
  };

  void
  add_rule(value match_pattern, value type_pattern, value predicate_identifier,
           value unpack_identifier);

  const rule&
  find_rule(value match_case, value type) const;

  private:
  opi::stl::deque<rule> m_cases_rules;
};



} // namespace opi
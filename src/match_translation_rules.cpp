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

#include "opium/scheme/translator/match_translation_rules.hpp"

#include "opium/stl/vector.hpp"


template <std::output_iterator<opi::value> Output>
void
_gather_literals(opi::value x, Output output) noexcept
{
  if (issym(x) and x != "_" and not std::isupper(sym_name(x)[0]))
    *output++ = x;
  else if (ispair(x))
  {
    _gather_literals(car(x), output);
    _gather_literals(cdr(x), output);
  }
}


void
opi::match_translation_rules::add_rule(value match_pattern, value type_pattern,
                                       value predicate_identifier,
                                       value unpack_identifier)
{
  stl::vector<value> matchliterals, typeliterals;
  _gather_literals(match_pattern, std::back_inserter(matchliterals));
  _gather_literals(type_pattern, std::back_inserter(typeliterals));
  m_cases_rules.emplace_back(match {list(matchliterals), match_pattern},
                              match {list(typeliterals), type_pattern},
                              predicate_identifier, unpack_identifier);
}


const opi::match_translation_rules::rule&
opi::match_translation_rules::find_rule(value match_case, value type) const
{
  const auto it = std::ranges::find_if(m_cases_rules, [&](const auto &rule) {
    return rule.ctor_match(match_case) and rule.type_match(type);
  });
  if (it != m_cases_rules.end())
    return *it;

  throw bad_code {
      std::format("No handler for case {} with type {}", match_case, type)};
}

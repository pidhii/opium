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


#include "opium/lisp_reader.hpp"
#include "opium/lisp_parser.hpp"


opi::lisp_reader::lisp_reader(opi::lisp_parser &parser)
: m_parser {parser}
{ }


void
opi::lisp_reader::operator << (const std::string &input)
{
  // Convert input string into tokens
  const stl::vector<lisp_parser::token> tokens = m_parser.tokenize(input);
  m_tokens.insert(m_tokens.end(), tokens.begin(), tokens.end());

  // Parse all available expressions from accumulated tokens
  size_t cursor = 0;
  while (true)
  {
    // Try parsing a single expression
    try { m_values.push_back(m_parser.parse_tokens(m_tokens, cursor)); }
    catch (parse_error &exn) // Not enough tokens to prouce an expression
    { break; }
  }

  // Erase consumed tokens
  m_tokens.erase(m_tokens.begin(), m_tokens.begin() + cursor);
}


bool
opi::lisp_reader::operator >> (opi::value &result)
{
  if (m_values.empty())
    return false;
  result = m_values.front();
  m_values.pop_front();
  return true;
}

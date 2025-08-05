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

#include "opium/lisp_parser.hpp"
#include "opium/stl/deque.hpp"
#include "opium/value.hpp"

#include <string>
#include <vector>

/**
 * \file lisp_reader.hpp
 * Lisp reader implementation
 * 
 * This file defines a reader for Lisp-style expressions that allows
 * gradual parsing of text fragments.
 * 
 * \ingroup lisp
 */


namespace opi {

/**
 * Utility class allowing gradual parsing of text fragments into LISP
 * 
 * \ingroup lisp
 */
class lisp_reader {
  public:
  lisp_reader(lisp_parser &parser);

  void
  operator << (const std::string &input);

  bool
  operator >> (value &result);

  private:
  lisp_parser &m_parser;
  stl::vector<lisp_parser::token> m_tokens;
  opi::stl::deque<value> m_values;
}; // class opi::lisp_reader

} // namespace opi

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

#include <string_view>
#include <string>
#include <format>


namespace opi {

class symbol_generator {
  public:
  symbol_generator(size_t &counter, std::string_view format = "_Uid{}")
  : m_format {format}, m_counter {counter}
  { }

  value
  operator () ()
  {
    m_counter ++;
    return sym(std::vformat(m_format, std::make_format_args(m_counter)));
  }

  value
  operator () (std::string_view format)
  {
    m_counter ++;
    return sym(std::vformat(format, std::make_format_args(m_counter)));
  }

  private:
  const std::string m_format;
  size_t &m_counter;
}; // class opi::symbol_generator

} // namespace opi
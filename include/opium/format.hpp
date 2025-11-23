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

#include <sstream>
#include <format>

/**
 * \file format.hpp
 * Formatting utilities
 * 
 * This file defines utilities for formatting values as strings.
 * 
 * \ingroup utils
 */


namespace opi {

/**
 * \namespace opi::detail
 * Implementation details
 */
namespace detail {

/**
 * Format a single value to an output stream
 * 
 * \tparam Os Output stream type
 * \tparam Head Value type
 * \param os Output stream
 * \param head Value to format
 * 
 * \ingroup utils
 */
template <typename Os, typename Head>
void
format(Os &os, Head&& head)
{ os << std::forward<Head>(head); }

/**
 * Format multiple values to an output stream
 * 
 * \tparam Os Output stream type
 * \tparam Head First value type
 * \tparam Tail Rest of value types
 * \param os Output stream
 * \param head First value to format
 * \param tail Rest of values to format
 * 
 * \ingroup utils
 */
template <typename Os, typename Head, typename ...Tail>
void
format(Os &os, Head&& head, Tail ...tail)
{
  os << std::forward<Head>(head);
  return format(os, std::forward<Tail>(tail)...);
}

} // namespace opi::detail

} // namespace opi


namespace std {

/**
 * Formatter for opi::value
 * 
 * \ingroup utils
 */
template <>
struct formatter<opi::value, char> {
  enum class style { write, display, print } style = style::print;
  unsigned long n = -1;
  bool colored = false;

  template <class ParseContext>
  constexpr ParseContext::iterator
  parse(ParseContext &ctx)
  {
    auto it = ctx.begin();
    while (it != ctx.end())
    {
      if (*it == 'w')
      {
        style = style::write;
        it++;
      }

      if (*it == 'd')
      {
        style = style::display;
        it++;
      }

      if (*it == 'c')
      {
        colored = true;
        it++;
      }

      if (*it == 'r')
      {
        colored = false;
        it++;
      }

      if (*it == '#')
      {
        it++;
        n = 0;
        while (*it >= '0' and *it <= '9')
        {
          n *= 10;
          n += *it - '0';
          it += 1;
        }
      }

      break;
    }
    if (it != ctx.end() and *it != '}')
      throw std::format_error {"Invalid format arguments for opi::value"};

    return it;
  }

  template <class FmtContext>
  FmtContext::iterator
  format(opi::value x, FmtContext &ctx) const
  {
    std::ostringstream buffer;
    const opi::printer &p = colored ? opi::colorized_printer : opi::raw_printer;
    switch (style)
    {
      case style::write: p.write(buffer, x, n); break;
      case style::display: p.display(buffer, x, n); break;
      case style::print: p.print(buffer, x, n); break;
    }
    return std::ranges::copy(std::move(buffer).str(), ctx.out()).out;
  }
};

} // namespace std

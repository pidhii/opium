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


#include "opium/utilities/execution_timer.hpp"
#include "opium/value.hpp"

#include <vector>
#include <string>


enum class mode { write, display, print };

// ANSI color codes for rainbow parentheses
const std::vector<std::string> PAREN_COLORS = {
  "\e[31m", // Red
  "\e[33m", // Yellow
  "\e[32m", // Green
  "\e[36m", // Cyan
  "\e[34m", // Blue
  "\e[35m"  // Magenta
};

const std::string RESET_COLOR = "\033[0m";

static void
_print(mode mode, std::ostream &os, opi::value val, opi::value mem,
       int maxdepth, int depth)
{
  OPI_FUNCTION_BENCHMARK

  using namespace opi;

  switch (opi::tag(val))
  {
    case tag::nil:
      os << (mode == mode::write ? "'()" : "()");
      break;

    case tag::boolean:
      os << (val == True ? "#t" : "#f");
      break;

    case tag::sym:
      if (mode == mode::write)
        os << '\'';
      os << sym_name(val);
      break;

    case tag::str:
      if (mode != mode::display)
        os << '"';
      for (size_t i = 0; i < str_view(val).size(); ++i)
      {
        const char c = str_view(val)[i];
        switch (c)
        {
          case '"':
          case '\\':
            os.put('\\');
            os.put(c);
            break;

          case '\a':
          case '\b':
          case '\f':
          case '\n':
          case '\r':
          case '\t':
          case '\v':
          case '\e':
            os << std::format("\\x{:02x}", int(c));
            break;

          default:
            os.put(c);
        }
      }
      if (mode != mode::display)
        os << '"';
      break;

    case tag::num:
      os << num_val(val);
      break;

    case tag::ptr:
      os << ptr_val(val);
      break;

    case tag::pair: {
      // Get color for current nesting level
      const std::string color = PAREN_COLORS[depth % PAREN_COLORS.size()];
      
      if (maxdepth >= 0 and depth >= maxdepth)
      {
        os << color << "(" << RESET_COLOR << "..." << color << ")"
           << RESET_COLOR;
        return;
      }

      // Momorize the pair so we dont print it multiple times in case of
      // self-referencing structures
      if (memq(val, mem))
      {
        os << "...";
        return;
      }
      mem = cons(val, mem);

      // Print opening parenthesis with color
      os << color << '(' << RESET_COLOR;
      
      _print(mode, os, car(val), mem, maxdepth, depth + 1);
      value elt = nil;
      for (elt = cdr(val); ispair(elt); elt = cdr(elt))
      {
         // Similar trick about self-referencing
        if (memq(elt, mem))
        {
          os << " ..." << color << ")" << RESET_COLOR;
          return;
        }
        mem = cons(elt, mem);

        // Normal printing
        os << ' ';
        _print(mode, os, car(elt), mem, maxdepth, depth + 1);
      }
      if (is(elt, nil))
        os << color << ')' << RESET_COLOR;
      else
        os << " . " << elt << color << ')' << RESET_COLOR;
      break;
    }
  }
}


void
opi::write(std::ostream &os, const opi::value &val, int maxdepth)
{ _print(mode::write, os, val, opi::nil, maxdepth, 0); }

void
opi::display(std::ostream &os, const opi::value &val, int maxdepth)
{ _print(mode::display, os, val, opi::nil, maxdepth, 0); }

void
opi::print(std::ostream &os, const opi::value &val, int maxdepth)
{ _print(mode::print, os, val, opi::nil, maxdepth, 0); }

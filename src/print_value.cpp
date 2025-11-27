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


#include "opium/predicate_runtime.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/value.hpp"

#include <vector>
#include <string>


const opi::printer::color_palette opi::printer::default_palette {
    {
        "\e[31m", // Red
        "\e[33m", // Yellow
        "\e[32m", // Green
        "\e[36m", // Cyan
        "\e[34m", // Blue
        "\e[35m", // Magenta
    },
    "\e[38;5;11;48;5;8;1m",
    "\e[38;5;10;48;5;8m",
    "\e[38;5;12;48;5;8m",
    "\e[38;5;15;48;5;8m",
    "\e[38;5;9m;48;5;8m",
    "\e[38;5;11;48;5;8;1m"};


const opi::printer opi::colorized_printer {opi::printer::default_palette};
const opi::printer opi::raw_printer;


enum class mode {
  write,
  display,
  print
};


static void
_print(mode mode, std::ostream &os, opi::value val, opi::value mem,
       int maxdepth, int depth, const opi::printer::color_palette &cp)
{
  using namespace opi;

  OPI_FUNCTION_BENCHMARK

#define RESET_COLOR(c) (c.empty() ? "" : "\e[0m")
#define BEGC(name) cp.name##_color
#define ENDC(name) RESET_COLOR(cp.name##_color)

  switch (opi::tag(val))
  {
    case tag::nil:
      if (mode == mode::write)
        os << '\'';
      os << BEGC(nil) << "()" << ENDC(nil);
      break;

    case tag::boolean:
      os << BEGC(bool) << (val == True ? "#t" : "#f") << ENDC(bool);
      break;

    case tag::sym:
      if (mode == mode::write)
        os << '\'';
      os << BEGC(symbol) << sym_name(val) << ENDC(symbol);
      break;

    case tag::str:
      if (mode != mode::display)
        os << BEGC(string) << '"';
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
        os << '"' << ENDC(string);
      break;

    case tag::num:
      os << BEGC(number) << num_val(val) << ENDC(number);
      break;

    case tag::ptr:
      os << BEGC(pointer) << ptr_val(val) << ENDC(pointer);
      break;

    case tag::pair: {
      if (car(val) == cell_tag)
      {
        os << "<cell:" << cdr(val) << ">";
        return;
      }

      // Get color for current nesting level
      const std::string_view pcolor =
          cp.parent_colors.empty()
              ? std::string_view("")
              : cp.parent_colors[depth % cp.parent_colors.size()];

      if (maxdepth >= 0 and depth >= maxdepth)
      {
        os << pcolor << "(" << RESET_COLOR(pcolor) << "..." << pcolor << ")"
           << RESET_COLOR(pcolor);
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
      os << pcolor << '(' << RESET_COLOR(pcolor);
      
      _print(mode, os, car(val), mem, maxdepth, depth + 1, cp);
      value elt;
      for (elt = cdr(val); ispair(elt) and car(elt) != cell_tag; elt = cdr(elt))
      {
         // Similar trick about self-referencing
        if (memq(elt, mem))
        {
          os << " ..." << pcolor << ")" << RESET_COLOR(pcolor);
          return;
        }
        mem = cons(elt, mem);

        // Normal printing
        os << ' ';
        _print(mode, os, car(elt), mem, maxdepth, depth + 1, cp);
      }
      if (is(elt, nil))
        os << pcolor << ')' << RESET_COLOR(pcolor);
      else
      {
        os << " . ";
        _print(mode, os, elt, mem, maxdepth, depth + 1, cp);
        os << pcolor << ')' << RESET_COLOR(pcolor);
      }
      break;
    }
  }
}

void
opi::printer::write(std::ostream &os, opi::value x, int maxdepth) const
{ _print(mode::write, os, x, nil, maxdepth, 0, palette); }

void
opi::printer::write(std::ostream &os, opi::value x) const
{ write(os, x, default_maxdepth); }

void
opi::printer::display(std::ostream &os, opi::value x, int maxdepth) const
{ _print(mode::display, os, x, nil, maxdepth, 0, palette); }

void
opi::printer::display(std::ostream &os, opi::value x) const
{ display(os, x, default_maxdepth); }

void
opi::printer::print(std::ostream &os, opi::value x, int maxdepth) const
{ _print(mode::print, os, x, nil, maxdepth, 0, palette); }

void
opi::printer::print(std::ostream &os, opi::value x) const
{ print(os, x, default_maxdepth); }

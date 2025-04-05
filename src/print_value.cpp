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

void
_print(mode mode, std::ostream &os, opi::value val, opi::value mem,
       int depth = 0)
{
  using namespace opi;

  switch (val->t)
  {
    case tag::nil:
      os << (mode == mode::write ? "'()" : "()");
      break;

    case tag::boolean:
      os << (val->boolean ? "#t" : "#f");
      break;

    case tag::sym:
      if (mode == mode::write)
        os << '\'';
      os.write(val->sym.data, val->sym.len);
      break;

    case tag::str:
      if (mode != mode::display)
        os << '"';
      for (size_t i = 0; i < val->sym.len; ++i)
      {
        const char c = val->str.data[i];
        if (c == '"')
          os.put('\\');
        os.put(c);
      }
      if (mode != mode::display)
        os << '"';
      break;

    case tag::num:
      os << val->num;
      break;

    case tag::ptr:
      os << val->ptr;
      break;

    case tag::pair: {
      // Momorize the pair so we dont print it multiple times in case of
      // self-referencing structures
      if (memq(val, mem))
      {
        os << "...";
        return;
      }
      mem = cons(val, mem);

      // Get color for current nesting level
      std::string color = PAREN_COLORS[depth % PAREN_COLORS.size()];
      
      // Print opening parenthesis with color
      os << color << '(' << RESET_COLOR;
      
      _print(mode, os, car(val), mem, depth + 1);
      value elt = nil;
      for (elt = cdr(val); elt->t == tag::pair; elt = cdr(elt))
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
        _print(mode, os, car(elt), mem, depth + 1);
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
opi::write(std::ostream &os, const opi::value &val)
{ _print(mode::write, os, val, opi::nil, 0); }

void
opi::display(std::ostream &os, const opi::value &val)
{ _print(mode::display, os, val, opi::nil, 0); }

void
opi::print(std::ostream &os, const opi::value &val)
{ _print(mode::print, os, val, opi::nil, 0); }

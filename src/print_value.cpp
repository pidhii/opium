#include "opium/value.hpp"


enum class mode { write, display, print };

void
_print(mode mode, std::ostream &os, opi::value val, opi::value mem)
{
  using namespace opi;

  switch (val->t)
  {
    case tag::nil:
      os << (mode == mode::write ? "'()" : "nil");
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

      os << '(';
      _print(mode, os, car(val), mem);
      value elt = nil;
      for (elt = cdr(val); elt->t == tag::pair; elt = cdr(elt))
      { // Similar trick about self-referencing
        if (memq(elt, mem))
        {
          os << "...)";
          return;
        }
        mem = cons(elt, mem);
        // Normal printing
        os << ' ';
        _print(mode, os, car(elt), mem);
      }
      if (is(elt, nil))
        os << ')';
      else
        os << " . " << elt << ')';
      break;
    }
  }
}


void
opi::write(std::ostream &os, const opi::value &val)
{ _print(mode::write, os, val, opi::nil); }

void
opi::display(std::ostream &os, const opi::value &val)
{ _print(mode::display, os, val, opi::nil); }

void
opi::print(std::ostream &os, const opi::value &val)
{ _print(mode::print, os, val, opi::nil); }

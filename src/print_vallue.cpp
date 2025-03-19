#include "opium/value.hpp"


static void
_print(std::ostream &os, opi::value val, opi::value mem)
{
  using namespace opi;

  switch (val->t)
  {
    case tag::nil:
      os << "nil";
      break;

    case tag::boolean:
      os << (val->boolean ? "#t" : "#f");
      break;

    case tag::sym:
      os.write(val->sym.data, val->sym.len);
      break;

    case tag::str:
      os << '"';
      for (size_t i = 0; i < val->sym.len; ++i)
      {
        const char c = val->str.data[i];
        if (c == '"')
          os.put('\\');
        os.put(c);
      }
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
      _print(os, car(val), mem);
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
        _print(os, car(elt), mem);
      }
      if (is(elt, nil))
        os << ')';
      else
        os << " . " << elt << ')';
      break;
    }
  }
}


std::ostream&
operator << (std::ostream &os, const opi::value &val)
{
  _print(os, val, opi::nil);
  return os;
}

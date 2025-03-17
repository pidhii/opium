#include "opium/value.hpp"

std::ostream&
operator << (std::ostream &os, const opi::value val)
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

    case tag::pair: {
      os << '(' << car(val);
      value elt = nil;
      for (elt = cdr(val); elt->t == tag::pair; elt = cdr(elt))
        os << ' ' << car(elt);
      if (is(elt, nil))
        os << ')';
      else
        os << " . " << elt << ')';
      break;
    }
  }

  return os;
}


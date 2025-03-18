#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_set.hpp"


#define MAX_REPETITIONS 5

static void
_print(std::ostream &os, const opi::value &val,
       opi::unordered_multiset<opi::value> &mem)
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
      if (mem.count(val) > MAX_REPETITIONS)
      {
        os << "<pair @ " << &val << ">";
        return;
      }
      else
        // Momorize the pair so we dont print it multiple times in case of
        // self-referencing structures
        mem.insert(val);

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
}


std::ostream&
operator << (std::ostream &os, const opi::value &val)
{
  opi::unordered_multiset<opi::value> mem;
  _print(os, val, mem);
  return os;
}

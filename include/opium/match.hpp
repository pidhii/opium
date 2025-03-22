#pragma once

#include "opium/value.hpp"

#include <concepts>


namespace opi {


struct default_match_variable {
  bool
  operator () (const char *_str, size_t _len) const noexcept
  { return true; }
};


namespace detail {
struct set_value {
  value val = nil;
  bool isset = false;

  void
  operator () (value val) noexcept
  {
    val = val;
    isset = true;
  }
};
}


template <typename Cont, typename IsVariable = default_match_variable>
void
match(value alist, value pat, value expr, Cont cont,
      const IsVariable &isvar = IsVariable { })
requires std::predicate<IsVariable, const char*, size_t> and
         std::regular_invocable<Cont, value>
{
  switch (expr->t)
  {
    case tag::sym:
      if (isvar(expr->sym.data, expr->sym.len))
        return cont(cons(cons(expr, pat), alist));

    case tag::nil:
    case tag::num:
    case tag::ptr:
    case tag::str:
    case tag::boolean:
      if (equal(expr, pat))
        return cont(alist);
      break;

    case tag::pair:
      if (pat->t == tag::pair)
      {
        detail::set_value matchresult;
        match(alist, cdr(pat), cdr(expr), matchresult, isvar);
        if (matchresult.isset)
          return match(matchresult.val, car(pat), car(expr), cont, isvar);
      }
  }
}


} // namespace opi

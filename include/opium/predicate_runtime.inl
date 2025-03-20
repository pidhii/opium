// Template implementation of opium/predicate_runtime.hpp members 
#pragma once

#include "opium/predicate_runtime.hpp"

namespace opi {
namespace detail {

template <unbound_variable_handler UVHandler>
struct _reconstructor {
  UVHandler uvhandler;
  opi::unordered_map<cell *, value> mem;

  template <typename T>
  _reconstructor(T &&uvhandler): uvhandler {std::forward<T>(uvhandler)} { }

  opi::value
  _reconstruct(opi::value x)
  {
    if (x->t == opi::tag::pair)
    {
      if (opi::issym(car(x), "__cell"))
        return _reconstruct(static_cast<opi::cell *>(x->cdr->ptr));
      else
        return cons(_reconstruct(car(x)), _reconstruct(cdr(x)));
    }
    else
      return x;
  }

  opi::value
  _reconstruct(opi::cell *x)
  {
    x = find(x);

    const auto it = mem.find(x);
    if (it != mem.end())
      return it->second;

    switch (x->kind)
    {
      case opi::cell::kind::value:
        if (x->val->t == opi::tag::pair)
        {
          const opi::value val = opi::cons(opi::nil, opi::nil);
          mem.emplace(x, val);
          val->car = &*_reconstruct(car(x->val));
          val->cdr = &*_reconstruct(cdr(x->val));
          return val;
        }
        else
          return x->val;

      case opi::cell::kind::variable:
        return uvhandler(x);
    }
    std::terminate();
  }

}; // struct opi::detail::_reconstructor

} // namespace opi::detail


template <unbound_variable_handler UVHandle>
value
reconstruct(cell *x, UVHandle uvhandler)
{
  return detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}


template <unbound_variable_handler UVHandle>
value
reconstruct(value x, UVHandle uvhandler)
{
  return detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}

} // namespace opi
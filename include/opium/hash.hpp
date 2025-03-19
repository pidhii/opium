#pragma once

#include "opium/value.hpp"

#include <string_view>


namespace opi {

template <class T>
inline void
hash_combine(size_t& seed, const T& v)
{
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

} // namespace opi


namespace std {

template <>
struct hash<opi::value> {
  constexpr size_t operator () (const opi::value &x) const noexcept
  {
    using cstrhash = std::hash<std::string_view>;
    switch (x->t)
    {
      case opi::tag::nil: return 0;
      case opi::tag::sym: return cstrhash{}({x->sym.data, x->sym.len});
      case opi::tag::str: return cstrhash{}({x->str.data, x->str.len});
      case opi::tag::num: return std::hash<long double>{}(x->num);
      case opi::tag::ptr: return std::hash<void*>{}(x->ptr);
      case opi::tag::boolean: return std::hash<bool>{}(x->boolean);
      case opi::tag::pair: {
        size_t hsh = (*this)(opi::value {x->car});
        hash_combine(hsh, x->cdr);
        return hsh;
      }
    }

    std::terminate();
  };
};

} // namespace std

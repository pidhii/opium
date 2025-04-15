#pragma once

#include "opium/value.hpp"

#include <string_view>
#include <unordered_set>


namespace opi {

template <class T>
inline void
hash_combine(size_t &seed, const T &v)
{
  std::hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

} // namespace opi


namespace std {

template <>
struct hash<opi::value> {
  size_t
  operator () (const opi::value &x) const noexcept
  {
    std::unordered_set<void*> mem;
    return _hash(x, mem);
  };

  private:
  // Hash function implementation with proper handling of recursive structures
  size_t 
  _hash(const opi::value &x, std::unordered_set<void*> &mem) const
  {
    std::hash<std::string_view> cstrhash;
    switch (x->t)
    {
      case opi::tag::nil: return 0;
      case opi::tag::sym: return cstrhash({x->sym.data, x->sym.len});
      case opi::tag::str: return cstrhash({x->str.data, x->str.len});
      case opi::tag::num: return std::hash<long double> {}(x->num);
      case opi::tag::ptr: return std::hash<void *> {}(x->ptr);
      case opi::tag::boolean: return std::hash<bool> {}(x->boolean);
      case opi::tag::pair: {
        if (not mem.emplace(&*x).second)
          return 0;
        size_t hash = _hash(opi::value {x->car}, mem);
        hash ^= _hash(car<false>(x), mem) + 0x9e3779b9 + (hash<<6) + (hash>>2);
        return hash;
      }
      default:
       std::terminate();
    }
  }
};

} // namespace std

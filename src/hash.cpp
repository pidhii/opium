#include "opium/value.hpp"

#include <unordered_set>


size_t 
_hash(const opi::value &x, [[maybe_unused]] std::unordered_set<void*> &mem)
{
  std::hash<std::string_view> cstrhash;
  switch (opi::tag(x))
  {
    case opi::tag::nil: return 0;
    case opi::tag::sym: return std::hash<size_t> {}(size_t(x->sym.data));
    case opi::tag::str: return cstrhash({x->str.data, x->str.len});
    case opi::tag::num: return std::hash<long double> {}(x->num);
    case opi::tag::ptr: return std::hash<void *> {}(x->ptr);
    case opi::tag::boolean: return std::hash<bool> {}(x->boolean);
    case opi::tag::pair: {
#ifdef OPIUM_HASH_CACHING
      size_t hash = car(x)->hash;
      hash ^= cdr(x)->hash + 0x9e3779b9 + (hash<<6) + (hash>>2);
#else
      if (not mem.emplace(&*x).second)
        return 0;
      size_t hash = _hash(car(x), mem);
      hash ^= _hash(cdr<false>(x), mem) + 0x9e3779b9 + (hash<<6) + (hash>>2);
#endif
      return hash;
    }
    default:
      std::terminate();
  }
}

size_t 
opi::hash(const opi::value &x)
{
  std::unordered_set<void*> mem;
  return _hash(x, mem);
}

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_set.hpp"

using _pair_of_pointers = std::pair<void*, void*>;
using _memory_set = opi::stl::unordered_set<_pair_of_pointers>;


namespace std {
template <>
struct hash<_pair_of_pointers> {
  size_t
  operator () (const _pair_of_pointers p) const noexcept
  {
    std::hash<void*> ptrhash;
    size_t hash = ptrhash(p.first);
    opi::hash_combine(hash, p.second);
    return hash;
  }
};
}

static bool
_equal(opi::value a, opi::value b, _memory_set &mem)
{
  if (is(a, b))
    return true;

  if (a->t != b->t)
    return false;

  switch (a->t)
  {
    case opi::tag::sym:
      return a->sym.len == b->sym.len and
             std::strncmp(a->sym.data, b->sym.data, a->sym.len) == 0;

    case opi::tag::nil:
      return true;

    case opi::tag::num:
      return a->num == b->num;

    case opi::tag::ptr:
      return a->ptr == b->ptr;

    case opi::tag::str:
      return a->str.len == b->str.len and
             std::strncmp(a->str.data, b->str.data, a->str.len) == 0;

    case opi::tag::pair: {
      // Don't repeat test on same pairs of pairs (objects)
      if (not mem.emplace(&*a, &*b).second)
        return true;

      // Equal if both car and cdr are equal
      return _equal(opi::value {a->car}, opi::value {b->car}, mem) and
             _equal(opi::value {a->cdr}, opi::value {b->cdr}, mem);
    }

    case opi::tag::boolean:
      return a->boolean == b->boolean;
  }

  abort();
}

bool
opi::equal(value a, value b)
{
  _memory_set mem;
  return _equal(a, b, mem);
}

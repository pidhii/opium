#pragma once

#include "opium/memory.hpp"

#include <map>

namespace opi {
inline namespace stl {

template <
  typename Key,
  typename T,
  typename Compare = std::less<Key>
>
using map = std::map<Key, T, Compare, gc_allocator<std::pair<const Key, T>>>;

template <
  typename Key,
  typename T,
  typename Compare = std::less<Key>
>
using multimap = std::multimap<Key, T, Compare, gc_allocator<std::pair<const Key, T>>>;

} // namespace opi::stl
} // namespace opi

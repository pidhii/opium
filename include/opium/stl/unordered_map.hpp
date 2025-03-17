#pragma once

#include "opium/memory.hpp"

#include <unordered_map>

namespace opi {

template <
  typename Key,
  typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>
>
using unordered_map = std::unordered_map<Key, T, Hash, KeyEqual,
                                         gc_allocator<std::pair<const Key, T>>>;

template <
  typename Key,
  typename T,
  typename Hash = std::hash<Key>,
  typename KeyEqual = std::equal_to<Key>
>
using unordered_multimap = std::unordered_multimap<Key, T, Hash, KeyEqual,
                                         gc_allocator<std::pair<const Key, T>>>;

} // namespace opi

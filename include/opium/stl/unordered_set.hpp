#pragma once

#include "opium/memory.hpp"

#include <unordered_set>

namespace opi {
inline namespace stl {

template <typename T, typename Hash = std::hash<T>,
          typename Equal = std::equal_to<T>>
using unordered_set = std::unordered_set<T, Hash, Equal, gc_allocator<T>>;

template <typename T, typename Hash = std::hash<T>,
          typename Equal = std::equal_to<T>>
using unordered_multiset =
    std::unordered_multiset<T, Hash, Equal, gc_allocator<T>>;

} // namespace opi::stl
} // namespace opi

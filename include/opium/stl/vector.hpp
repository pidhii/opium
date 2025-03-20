#pragma once

#include "opium/memory.hpp"

#include <vector>


namespace opi {
inline namespace stl {

template <typename T>
using vector = std::vector<T, gc_allocator<T>>;

} // namespace opi::stl
} // namespace opi

#pragma once

#include "opium/memory.hpp"

#include <deque>

namespace opi {
inline namespace stl {

template <typename T>
using deque = std::deque<T, gc_allocator<T>>;

} // namespace opi::stl
} // namespace opi

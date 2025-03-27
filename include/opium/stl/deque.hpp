#pragma once

#include "opium/memory.hpp"

#include <deque>

namespace opi::stl {

template <typename T>
using deque = std::deque<T, gc_allocator<T>>;

} // namespace opi::stl

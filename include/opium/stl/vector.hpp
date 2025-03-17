#pragma once

#include "opium/memory.hpp"

#include <vector>

namespace opi {

template <typename T>
using vector = std::vector<T, gc_allocator<T>>;

} // namespace opi

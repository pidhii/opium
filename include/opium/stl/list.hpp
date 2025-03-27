#pragma once

#include "opium/memory.hpp"

#include <list>


namespace opi::stl {

template <typename T>
using list = std::list<T, gc_allocator<T>>;

} // namespace opi::stl
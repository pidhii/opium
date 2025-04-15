/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#pragma once

#include "opium/memory.hpp"

#include <unordered_set>

namespace opi::stl {

template <typename T, typename Hash = std::hash<T>,
          typename Equal = std::equal_to<T>>
using unordered_set = std::unordered_set<T, Hash, Equal, gc_allocator<T>>;

template <typename T, typename Hash = std::hash<T>,
          typename Equal = std::equal_to<T>>
using unordered_multiset =
    std::unordered_multiset<T, Hash, Equal, gc_allocator<T>>;

} // namespace opi::stl

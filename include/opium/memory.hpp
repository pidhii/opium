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

#include <gc.h>

#include <utility>
#include <type_traits>

/**
 * \file memory.hpp
 * Memory management utilities for the Opium library
 * 
 * This file provides memory allocation and management functions using the Boehm GC
 * garbage collector. It includes functions for creating objects and allocating memory
 * with automatic garbage collection.
 * 
 * \ingroup memory
 */

/**
 * \namespace opi
 * The main namespace for the Opium library
 */
namespace opi {

/**
 * Create a garbage-collected object
 * 
 * Allocates memory for an object of type T using the garbage collector
 * and constructs the object with the provided arguments.
 * 
 * \tparam T The type of object to create
 * \tparam Args Types of constructor arguments
 * \param args Constructor arguments
 * \return T* Pointer to the newly created object
 * 
 * \ingroup memory
 */
template <typename T, typename ...Args>
T*
make(Args&& ...args)
{
  T* obj = static_cast<T*>(GC_malloc(sizeof(T)));
  new (obj) T {std::forward<Args>(args)...};
  return obj;
}

/**
 * Create a garbage-collected object with atomic memory
 * 
 * Similar to make(), but uses GC_malloc_atomic which is more efficient
 * for objects that don't contain pointers to other garbage-collected objects.
 * 
 * \tparam T The type of object to create
 * \tparam Args Types of constructor arguments
 * \param args Constructor arguments
 * \return T* Pointer to the newly created object
 * 
 * \ingroup memory
 */
template <typename T, typename ...Args>
T*
make_atomic(Args&& ...args)
{
  T* obj = static_cast<T*>(GC_malloc_atomic(sizeof(T)));
  new (obj) T {std::forward<Args>(args)...};
  return obj;
}

/**
 * Allocate garbage-collected memory
 * 
 * \param size Size in bytes to allocate
 * \return void* Pointer to the allocated memory
 * 
 * \ingroup memory
 */
inline void*
allocate(size_t size)
{ return GC_malloc(size); }

/**
 * Allocate atomic garbage-collected memory
 * 
 * Allocates memory that is known not to contain pointers to
 * garbage-collected objects.
 * 
 * \param size Size in bytes to allocate
 * \return void* Pointer to the allocated memory
 * 
 * \ingroup memory
 */
inline void*
allocate_atomic(size_t size)
{ return GC_malloc_atomic(size); }


/**
 * Concept for raw memory allocators
 * 
 * Defines requirements for types that can be used as raw allocators.
 * A raw allocator must be callable with a size_t argument and return
 * a pointer that is convertible to void*.
 * 
 * \tparam T The allocator type
 * 
 * \ingroup memory
 */
template <typename T>
concept raw_allocator = requires(T a)
{
  { a(size_t{}) } -> std::convertible_to<void*>;
};

/**
 * Base class for garbage-collected allocators
 * 
 * Provides a standard allocator interface that uses the garbage collector
 * for memory management. Compatible with STL containers.
 * 
 * \tparam T The value type to allocate
 * \tparam RawAllocator The underlying raw allocator to use
 * 
 * \ingroup memory
 */
template <typename T, raw_allocator RawAllocator>
struct gc_allocator_base {
  using pointer = T*;
  using const_pointer = const T*;
  using void_pointer = void*;
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  
  // Rebind structure for allocator compatibility
  template <typename U>
  struct rebind {
    using other = gc_allocator_base<U, RawAllocator>;
  };
  
  // Allocator traits for C++17/C++20 compatibility
  using propagate_on_container_move_assignment = std::true_type;
  using is_always_equal = std::true_type;

  gc_allocator_base(const RawAllocator &rawalloc = RawAllocator { })
  : m_rawalloc {rawalloc}
  { }

  gc_allocator_base(const gc_allocator_base &other)
  : m_rawalloc {other.m_rawalloc}
  { }

  gc_allocator_base(gc_allocator_base &&other) noexcept
  : m_rawalloc {std::move(other.m_rawalloc)}
  { }

  gc_allocator_base &
  operator = (const gc_allocator_base &other)
  {
    m_rawalloc = other.m_rawalloc;
    return *this;
  }

  gc_allocator_base &
  operator = (gc_allocator_base &&other) noexcept
  {
    m_rawalloc = std::move(other.m_rawalloc);
    return *this;
  }

  template <typename U>
  gc_allocator_base(const gc_allocator_base<U, RawAllocator> &other)
  : m_rawalloc {other.raw_allocator()}
  { }

  const RawAllocator&
  raw_allocator() const noexcept
  { return m_rawalloc; }

  pointer
  allocate(size_type n)
  { return static_cast<T*>(m_rawalloc(n * sizeof(T))); }

  void
  deallocate(pointer p, [[maybe_unused]] size_type n)
  { GC_free(p); }

  // Equality comparison operators
  bool
  operator == (const gc_allocator_base &) const noexcept
  { return true; }

  bool
  operator != (const gc_allocator_base &) const noexcept
  { return false; }

  private:
  RawAllocator m_rawalloc;
}; // namespace opi::allocator

namespace detail {
struct allocate_wrapper {
  void* operator () (size_t nb) const noexcept { return opi::allocate(nb); }
}; // struct opi::allocate_wrapper

struct allocate_atomic_wrapper {
  void* operator () (size_t nb) const noexcept { return opi::allocate_atomic(nb); }
}; // struct opi::allocate_atomic_wrapper
}

template <typename T>
using gc_allocator = gc_allocator_base<T, detail::allocate_wrapper>;

template <typename T>
using atomic_gc_allocator = gc_allocator_base<T, detail::allocate_atomic_wrapper>;


} // namespace opi

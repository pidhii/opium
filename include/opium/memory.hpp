#pragma once

#include <gc.h>

#include <utility>


namespace opi {

template <typename T, typename ...Args>
T*
make(Args&& ...args)
{
  T* obj = static_cast<T*>(GC_malloc(sizeof(T)));
  new (obj) T {std::forward<Args>(args)...};
  return obj;
}

template <typename T, typename ...Args>
T*
make_atomic(Args&& ...args)
{
  T* obj = static_cast<T*>(GC_malloc_atomic(sizeof(T)));
  new (obj) T {std::forward<Args>(args)...};
  return obj;
}

inline void*
allocate(size_t size)
{ return GC_malloc(size); }

inline void*
allocate_atomic(size_t size)
{ return GC_malloc_atomic(size); }


template <typename T>
concept raw_allocator = requires(T a)
{
  { a(size_t{}) } -> std::convertible_to<void*>;
};

template <typename T, raw_allocator RawAllocator>
struct gc_allocator_base {
  using pointer = T*;
  using const_pointer = const T*;
  using void_pointer = void*;
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;

  gc_allocator_base(const RawAllocator &rawalloc = RawAllocator { })
  : m_rawalloc {rawalloc}
  { }

  gc_allocator_base(const gc_allocator_base &other)
  : m_rawalloc {other.m_rawalloc}
  { }

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

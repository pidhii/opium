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

#include "opium/value.hpp"


namespace opi {

/**
 * \name Cons-list view implementation
 * \{
 */

/**
 * Iterator for cons-lists
 * 
 * \ingroup core
 */
struct cons_list_iterator {
  using difference_type = ptrdiff_t;
  using value_type = value;

  cons_list_iterator(): m_l {nil} { }

  /**
   * Constructor
   * 
   * \param l List to iterate over
   */
  explicit
  cons_list_iterator(value l): m_l {l} { }

  /**
   * Dereference operator
   * 
   * \return Current element
   */
  value
  operator * () const noexcept
  { return car<false>(m_l); }

  /**
   * Pre-increment operator
   * 
   * \return Reference to this iterator
   */
  cons_list_iterator&
  operator ++ () noexcept
  { m_l = cdr<false>(m_l); return *this; }

  /**
   * Post-increment operator
   * 
   * \return New iterator pointing to the next element
   */
  cons_list_iterator
  operator ++ (int) noexcept
  { return cons_list_iterator {cdr(m_l)}; }

  bool
  operator == (const cons_list_iterator &other) const noexcept
  { return is(m_l, other.m_l); }

  private:
  value m_l; /**< Current list position */

  friend struct cons_list_sentinel;
};
static_assert(std::input_iterator<cons_list_iterator>);
static_assert(std::forward_iterator<cons_list_iterator>);


/**
 * Sentinel for cons-list iteration
 * 
 * \ingroup core
 */
struct cons_list_sentinel {
  using difference_type = ptrdiff_t;

  /**
   * Equality comparison
   * 
   * \param it Iterator to compare with
   * \return True if the iterator has reached the end
   */
  bool
  operator == (const cons_list_iterator &it) const noexcept
  { return it.m_l->t != tag::pair; }

  value
  operator * () const noexcept
  { return nil; }

  cons_list_sentinel&
  operator ++ () noexcept
  { return *this; }

  cons_list_sentinel
  operator ++ (int) noexcept
  { return cons_list_sentinel {}; }
};
static_assert(std::sentinel_for<cons_list_iterator, cons_list_sentinel>);


/**
 * Implementation of std::ranges::view interface for cons-lists
 *
 * \ingroup core
 */
struct list_view: std::ranges::view_interface<list_view> {
  list_view(value l): m_list {l} {}

  cons_list_iterator
  begin() const noexcept
  { return cons_list_iterator {m_list}; }

  cons_list_sentinel
  end() const noexcept
  { return cons_list_sentinel {}; }

  private:
  value m_list;
}; // struct opi::list_view
static_assert(std::ranges::input_range<list_view>);
static_assert(std::ranges::forward_range<list_view>);
static_assert(std::ranges::view<list_view>);

/**
 * Create a range over a list
 * 
 * \param l List to iterate over
 * \return Range over the list elements
 *
 * \ingroup core
 */
inline list_view
range(value l)
{ return list_view {l}; }

/** /} */

} // namespace opi

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


/**
 * \file predicate_runtime.ini
 * Template implementation of opium/predicate_runtime.hpp members 
 */
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/utilities/execution_timer.hpp"

#include <cassert>


namespace opi::detail {

/**
 * Helper struct for reconstructing values from cells
 * 
 * \tparam UVHandler Type of unbound variable handler
 */
template <unbound_variable_handler UVHandler>
struct _reconstructor {
  UVHandler uvhandler; /**< Handler for unbound variables */
  opi::stl::unordered_map<void *, value> mem; /**< Memoization map to avoid infinite recursion */

  /**
   * Constructor
   * 
   * \tparam T Type of unbound variable handler
   * \param uvhandler Handler for unbound variables
   */
  template <typename T>
  _reconstructor(T &&uvhandler): uvhandler {std::forward<T>(uvhandler)} { }

  /**
   * Reconstruct a value from a cell
   * 
   * \param x Cell to reconstruct from
   * \return Reconstructed value
   */
  opi::value
  _reconstruct(opi::cell *x)
  {
    x = find(x);

    switch (x->kind)
    {
      case opi::cell::kind::value:
        return _reconstruct(x->val);

      case opi::cell::kind::variable:
        return uvhandler(x);
    }
    std::terminate();
  }

  /**
   * Reconstruct a value
   * 
   * \param x Value to reconstruct
   * \return Reconstructed value
   */
  opi::value
  _reconstruct(opi::value x)
  {
    if (ispair(x))
    {
      value result = nil;
      while (ispair(x) and not is(car(x), opi::cell_tag))
      {
        auto it = mem.find(&*x);
        if (it != mem.end())
          return append_mut(result, it->second);

        value newx = cons(nil, nil);
        mem.emplace(&*x, newx);
        set_car(newx, _reconstruct(car(x)));
        copy_location(x, newx); // NOTE don't know, might be needed (apparently it is, needed or else we don't tracing inside function bodies)

        result = append_mut(result, newx);
        x = cdr(x);
      }

      if (ispair(x) and opi::is(car(x), opi::cell_tag))
        return append_mut(result, _reconstruct(static_cast<opi::cell *>(ptr_val(cdr(x)))));
      else
        return append_mut(result, x);
    }
    else
      return x;
  }
}; // struct opi::detail::_reconstructor

} // namespace opi::detail


template <opi::unbound_variable_handler UVHandle>
opi::value
opi::reconstruct(cell *x, UVHandle uvhandler)
{
  OPI_FUNCTION_BENCHMARK
  return opi::detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}


template <opi::unbound_variable_handler UVHandle>
opi::value
opi::reconstruct(value x, UVHandle uvhandler)
{
  OPI_FUNCTION_BENCHMARK
  return detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}

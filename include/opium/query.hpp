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

#include "opium/predicate_runtime.hpp"
#include "opium/stl/map.hpp"
#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/stl/unordered_set.hpp"
#include "opium/value.hpp"
#include "opium/exceptions.hpp"

#include <cstring>


namespace opi {

namespace detail {

/**
 * Comparison functor for symbols
 */
struct _compare_symbols {
  /**
   * Compare two symbols lexicographically
   * 
   * \param a First symbol
   * \param b Second symbol
   * \return True if a is lexicographically less than b
   * \throws bad_code If either a or b is not a symbol
   */
  bool
  operator () (const value &a, const value &b) const
  {
    if (not opi::issym(a) or not opi::issym(b))
      throw bad_code {
          std::format("expected symbols, got {} < {}", a, b), a};
    return sym_name(a) < sym_name(b);
  }
}; // class opi::detail::_compare

} // namespace opi::detail


/**
 * A map that summarizes determined values for variables in a predicate runtime
 */
class unified_determined_summary
: public opi::stl::map<value, opi::stl::unordered_set<value>,
                       detail::_compare_symbols> {
  public:
  /**
   * Constructor
   * 
   * \param prt Predicate runtime to summarize
   */
  unified_determined_summary(const predicate_runtime &prt)
  : m_prt {prt}, m_invoked {false}
  { }

  operator bool () const noexcept
  { return m_invoked; }

  /**
   * Collect determined values for all variables in the predicate runtime
   * 
   * For each variable, attempts to reconstruct its value. If successful,
   * adds the value to the set of determined values for that variable.
   */
  void
  operator () () 
  {
    m_invoked = true;
    for (const value var : m_prt.variables())
    {
      const value varval =
          reconstruct(m_prt.variable(var), [&](cell *) { return sym("<any>"); });
      this->operator[](var).emplace(varval);
    }
  }

  private:
  const predicate_runtime &m_prt; /**< Reference to the predicate runtime */
  bool m_invoked;
}; // class opi::unified_determined_summary

} // namespace opi

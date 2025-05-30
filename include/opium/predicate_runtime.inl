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
    if (x->t == opi::tag::pair)
    {
      value result = nil;
      while (x->t == opi::tag::pair and not issym(car(x), CELL))
      {
        auto it = mem.find(&*x);
        if (it != mem.end())
          return append_mut(result, it->second);

        value newx = cons(nil, nil);
        mem.emplace(&*x, newx);
        set_car(newx, _reconstruct(car(x)));
        copy_location(x, newx);

        result = append_mut(result, newx);
        x = cdr(x);
      }

      if (x->t == tag::pair and opi::issym(car(x), CELL))
        return append_mut(result, _reconstruct(static_cast<opi::cell *>(cdr(x)->ptr)));
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
  execution_timer _ {"reconstruct()"};
  return opi::detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}


template <opi::unbound_variable_handler UVHandle>
opi::value
opi::reconstruct(value x, UVHandle uvhandler)
{
  execution_timer _ {"reconstruct()"};
  return detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}


// #define USE_EQUIVALENCE

template <opi::nonterminal_variable_handler NTVHandler>
bool
opi::predicate_runtime::try_sign(const void *preduid, value signature,
                                 const predicate_runtime &prev,
                                 NTVHandler ntvhandler) noexcept
{
  for (const predicate_runtime *prt = &prev; prt; prt = prt->m_prev_frame)
  {
    if (preduid == prt->m_preduid)
    {
      assert(prt->m_preduid != nullptr);

#ifdef USE_EQUIVALENCE
      // const bool isequiv = equivalent(signature, prt->m_signature);
      // warning("test equivalence -> {}, where\na = {}\nb = {}", isequiv,
      //         reconstruct(signature, ignore_unbound_variables),
      //         reconstruct(prt->m_signature, ignore_unbound_variables));

      if (equivalent(signature, prt->m_signature))
      {
        // Do the actual match as we may be dealing with recursive types instead
        // if just non-terminals
        const bool ok = match_arguments(*this, signature, prt->m_signature);
        assert(ok and "Failed to match equivalent signatures");

        // Process non-terminal variables
        if constexpr (not std::is_same_v<NTVHandler, ignore_nonterminal_variables>)
        {
          for (const value var : variables())
          {
            // Variables present in signature but not bound by `match_arguments`
            // are regarded as non-terminal (computation of their type will not
            // terminate).
            // Use `reconstruct` to scan for (possibly) nested unbound variables
            // variables and trigger user-handler (`ntvhandler`) on each of them.
            reconstruct((*this)[var], [&](cell *x) {
              ntvhandler(*this, x);
              return nil;
            });
          }
        }
        return false; // Notify about signature clash
      }
#else
      predicate_runtime tmp;
      if (match_arguments(tmp, signature, prt->m_signature))
      {
        // Own cells from `tmp`
        std::ranges::copy(tmp.m_terms, std::back_inserter(m_terms));
        std::ranges::copy(tmp.m_varmap | std::views::values, std::back_inserter(m_terms));
        tmp.m_terms.clear();
        tmp.m_varmap.clear();

        // Process non-terminal variables
        if constexpr (not std::is_same_v<NTVHandler, ignore_nonterminal_variables>)
        {
          for (const value var : variables())
          {
            // Variables present in signature but not bound by `match_arguments`
            // are regarded as non-terminal (computation of their type will not
            // terminate).
            // Use `reconstruct` to scan for (possibly) nested unbound variables
            // variables and trigger user-handler (`ntvhandler`) on each of them.
            reconstruct((*this)[var], [&](cell *x) {
              ntvhandler(*this, x);
              return nil;
            });
          }
        }
        return false; // Notify about signature clash
      }
#endif
    }
  }

  // No chash encountered. Add this runtime into the chain and report success
  m_preduid = preduid;
  m_signature = signature;
  m_prev_frame = &prev;
  return true;
}

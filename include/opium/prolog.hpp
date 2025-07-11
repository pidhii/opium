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
#include "opium/format.hpp" // IWYU pragma: export
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/map.hpp"
#include "opium/exceptions.hpp"

#include <asm-generic/errno.h>
#include <concepts>
#include <string>
#include <cassert>
#include <ranges>
#include <filesystem>

/**
 * \file prolog.hpp
 * Prolog language implementation
 * 
 * This file defines the core classes for the Prolog language implementation,
 * including predicate representation and evaluation.
 * 
 * \ingroup prolog
 */


namespace opi {


/**
 * Predicate representation
 * 
 * \ingroup prolog
 */
class predicate {
  public:
  /**
   * Constructor
   * 
   * \param sig Signature of the predicate
   * \param body Body/rule of the predicate
   * 
   * \todo validate types
   */
  predicate(value sig, value body)
  : m_name {opi::car(sig)->sym.data},
    m_args {cdr(sig)},
    m_body {body}
  { }

  /**
   * Get name of the predicate
   * 
   * \return Name of the predicate
   */
  const std::string&
  name() const noexcept
  { return m_name; }

  value
  signature() const noexcept
  { return cons(sym(m_name), m_args); }

  /**
   * Get predicate body/rule
   * 
   * \return Predicate body/rule
   */
  value
  body() const noexcept
  { return m_body; }

  private:
  std::string m_name; /**< Name of the predicate */
  value m_args; /**< Arguments of the predicate */
  value m_body; /**< Body/rule of the predicate */
}; // class opi::predicate


/**
 * Concept for continuations in Prolog evaluation
 * 
 * \ingroup prolog
 */
template <typename Cont>
concept prolog_continuation = std::regular_invocable<Cont>;


class query_blame_list {
  public:
  query_blame_list(
      const stl::map<std::string, std::pair<size_t, value>> &blame_list)
  : m_blame_list {blame_list}
  { }

  bool
  get_blame(const std::filesystem::path &file_path, source_location &location)
  {
    const std::filesystem::path fullpath = std::filesystem::absolute(file_path);
    const auto it = m_blame_list.find(fullpath);
    if (it == m_blame_list.end())
      return false;
    return get_location(it->second.second, location);
  }

  private:
  stl::map<std::string, std::pair<size_t, value>> m_blame_list;
};

/**
 * Prolog evaluator
 * 
 * \ingroup prolog
 */
class prolog {
  public:
  struct error: public bad_code {
    using bad_code::bad_code;
  };

  prolog();

  /**
   * Add a predicate to the evaluator
   * 
   * \param sig Signature of the predicate
   * \param body Body/rule of the predicate
   * \return Reference to the added predicate
   * 
   * \todo validate types
   */
  const predicate&
  add_predicate(value sig, value body);

  const predicate&
  add_predicate(const predicate &pred);

  std::ranges::view auto
  predicate_branches(const std::string &name) const;

  std::ranges::range auto
  predicates() const noexcept
  { return m_db; }

  template <
    prolog_continuation Cont,
    nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables>
  void
  make_true(predicate_runtime &ert, value e, Cont cont,
            NTVHandler ntvhandler = NTVHandler {}) const;

  query_blame_list
  blame_list() const
  { return {m_last_expr}; }

  private:
  // Helper function to track the deepest evaluated expression. Tracking is done
  // separately for each file (determined from the location source of the `expr`
  // parameter)
  void
  _update_last_expr(size_t depth, value expr) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_if_true(predicate_runtime &ert, value cond, value thenbr, value elsebr,
                Cont cont, NTVHandler ntvhandler) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_and_true(predicate_runtime &ert, value clauses, Cont cont,
                 NTVHandler ntvhandler) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_or_true(predicate_runtime &ert, value clauses, Cont cont,
                NTVHandler ntvhandler) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_predicate_true(predicate_runtime &ert, const predicate &pred,
                       value eargs, Cont cont, NTVHandler ntvhandler) const;

  private:
  mutable size_t m_depth;
  mutable stl::map<std::string, std::pair<size_t, value>> m_last_expr;
  opi::stl::unordered_multimap<std::string, predicate> m_db; /**< Database of predicates */
}; // class opi::prolog


} // namespace opi


#include "opium/prolog.inl"

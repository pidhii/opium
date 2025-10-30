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

#include "opium/source_location.hpp"
#include "opium/value.hpp"
#include "opium/format.hpp" // IWYU pragma: export
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"
#include "opium/exceptions.hpp"

#include <asm-generic/errno.h>
#include <concepts>
#include <iterator>
#include <string>
#include <cassert>
#include <ranges>

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


struct trace_node {
  source_location location;
  stl::vector<trace_node*> children;
};


namespace detail {
template <typename Collect>
struct trace_collector {
  template <typename Container>
  void
  follow_the_trace(trace_node *root, Container &trace)
  {
    if (root->children.empty())
      m_collect(std::cref(trace).get());
    else
    {
      trace.push_back(root->location);
      for (trace_node *branch : root->children)
        follow_the_trace(branch, trace);
      trace.pop_back();
    }
  }

  Collect m_collect;
}; // struct opi::detail::trace_collector
} // namespace opi::detail

template <typename Container = stl::vector<source_location>, typename Collect>
void
scan_traces(trace_node *root, Collect &&collect)
{
  detail::trace_collector<Collect> tc {std::forward<Collect>(collect)};
  Container trace;
  tc.follow_the_trace(root, trace);
}



template <typename Guide>
concept prolog_guide = requires(Guide g, value v, const trace_node *t) {
  { g(v, t) } -> std::convertible_to<bool>;
};

struct bruteforce_query {
  bool operator () (value, const trace_node *) { return true; }
};
static_assert(prolog_guide<bruteforce_query>);




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
    nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables,
    prolog_guide Guide = bruteforce_query>
  void
  make_true(predicate_runtime &ert, value e, Cont cont,
            NTVHandler ntvhandler = NTVHandler {},
            Guide guide = Guide {}) const;

  trace_node *
  query_trace() const
  { return m_trace; }

  private:
  // Helper function to track the deepest evaluated expression. Tracking is done
  // separately for each file (determined from the location source of the `expr`
  // parameter)
  void
  _trace_expr(value expr) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
            prolog_guide Guide>
  void
  _make_if_true(predicate_runtime &ert, value cond, value thenbr, value elsebr,
                Cont cont, NTVHandler ntvhandler, Guide guide) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
            prolog_guide Guide>
  void
  _make_and_true(predicate_runtime &ert, value clauses, Cont cont,
                 NTVHandler ntvhandler, Guide guide) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
            prolog_guide Guide>
  void
  _make_or_true(predicate_runtime &ert, value clauses, Cont cont,
                NTVHandler ntvhandler, Guide guide) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
            prolog_guide Guide>
  void
  _make_predicate_true(predicate_runtime &ert, const predicate &pred,
                       value eargs, Cont cont, NTVHandler ntvhandler,
                       Guide guide) const;

  private:
  mutable size_t m_depth;
  mutable trace_node *m_trace;
  mutable bool m_cut, m_cutpred;
  opi::stl::unordered_multimap<std::string, predicate> m_db; /**< Database of predicates */
}; // class opi::prolog


} // namespace opi


#include "opium/prolog.inl"

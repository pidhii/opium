#pragma once

#include "opium/value.hpp"
#include "opium/format.hpp" // IWYU pragma: export
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"

#include <asm-generic/errno.h>
#include <concepts>
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
    m_body {body}
  {
    for (opi::value x : opi::range(opi::cdr(sig)))
      m_args.emplace_back(x);
  }

  /**
   * Get name of the predicate
   * 
   * \return Name of the predicate
   */
  const std::string&
  name() const noexcept
  { return m_name; }

  /**
   * Get predicate arguments (as std::range)
   * 
   * \return Range of predicate arguments
   */
  std::ranges::range auto
  arguments() const noexcept
  { return m_args; }

  value
  argument(size_t i) const
  { return m_args.at(i); }

  value
  signature() const noexcept
  { return cons(sym(m_name), list(m_args)); }

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
  opi::stl::vector<value> m_args; /**< Arguments of the predicate */
  value m_body; /**< Body/rule of the predicate */
}; // class opi::predicate


/**
 * Concept for continuations in Prolog evaluation
 * 
 * \ingroup prolog
 */
template <typename Cont>
concept prolog_continuation = std::regular_invocable<Cont>;


/**
 * Prolog evaluator
 * 
 * \ingroup prolog
 */
class prolog {
  public:
  struct error: public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

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

  template <
    prolog_continuation Cont,
    nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables>
  void
  make_true(predicate_runtime &ert, value e, Cont cont,
            NTVHandler ntvhandler = NTVHandler {}) const;

  private:
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
  opi::stl::unordered_multimap<std::string, predicate> m_db; /**< Database of predicates */
}; // class opi::prolog


} // namespace opi


#include "opium/prolog.inl"

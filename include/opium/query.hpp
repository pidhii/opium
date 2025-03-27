#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/stl/map.hpp"
#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/stl/unordered_set.hpp"
#include "opium/value.hpp"

#include <stdexcept>
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
   * \throws std::runtime_error If either a or b is not a symbol
   */
  bool
  operator () (const value &a, const value &b) const
  {
    if (a->t != opi::tag::sym or b->t != opi::tag::sym)
      throw std::runtime_error {
          std::format("expected symbols, got {} < {}", a, b)};
    return std::strcmp(a->sym.data, b->sym.data) < 0;
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
  unified_determined_summary(const predicate_runtime &prt): m_prt {prt} { }

  public:
  /**
   * Collect determined values for all variables in the predicate runtime
   * 
   * For each variable, attempts to reconstruct its value. If successful,
   * adds the value to the set of determined values for that variable.
   */
  void
  operator () () 
  {
    for (const value var : m_prt.variables())
    {
      try {
        const value varval =
            reconstruct(m_prt[var], [&](cell *) { return sym("<any>"); });
        this->operator[](var).emplace(varval);
      }
      catch (const reconstruction_error&)
      { /* just ignore the value */; }
    }
  }

  private:
  const predicate_runtime &m_prt; /**< Reference to the predicate runtime */
}; // class opi::unified_determined_summary

} // namespace opi

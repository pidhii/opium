#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/stl/map.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/format.hpp"
#include "opium/value.hpp"

#include <stdexcept>
#include <cstring>


namespace opi {

namespace detail {

struct _compare_symbols {
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


class unified_determined_summary
    : public opi::map<value, opi::unordered_set<value>,
                      detail::_compare_symbols> {
  public:
  unified_determined_summary(const predicate_runtime &prt): m_prt {prt} { }

  public:
  void
  operator () () 
  {
    for (const value var : m_prt.variables())
    {
      try {
        const value varval = reconstruct(m_prt[var]);
        this->operator[](var).emplace(varval);
      }
      catch (const reconstruction_error&)
      { /* just ignore the value */; }
    }
  }

  private:
  const predicate_runtime &m_prt;
}; // class opi::unified_determined_summary

} // namespace opi

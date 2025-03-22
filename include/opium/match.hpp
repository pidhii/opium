#pragma once

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/format.hpp"

#include <concepts>


namespace opi {


template <typename T>
concept value_mapping = requires(T &x, value k)
{
  { x.insert(std::make_pair(k, k)) };
};

class match {
  public:
  match(value literals, value pattern)
  : m_literals {literals},
    m_pattern {pattern}
  { }

  template <value_mapping Mapping>
  bool
  operator () (value expr, Mapping &result)
  { return _match(m_pattern, expr, result); }

  bool
  operator () (value expr)
  {
    opi::unordered_map<value, value> _;
    return _match(m_pattern, expr, _);
  }

  private:
  template <value_mapping Mapping>
  bool
  _match(value pat, value expr, Mapping &result)
  {
    switch (pat->t)
    {
      case tag::sym:
        if (member(pat, m_literals))
          return equal(pat, expr);
        else
        {
          if (result.contains(pat))
            return equal(result.at(pat), expr);
          result.insert({pat, expr});
          return true;
        }

      case tag::pair:
        return expr->t == tag::pair and
               _match(car(pat), car(expr), result) and
               _match(cdr(pat), cdr(expr), result);

      default:
        return equal(pat, expr);
    }
  }

  private:
  value m_literals;
  value m_pattern;
};


} // namespace opi

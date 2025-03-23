#pragma once

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/format.hpp"

#include <concepts>
#include <stdexcept>


namespace opi {


template <typename T>
concept value_mapping = requires(T &x, value k)
{
  { x.contains(k) } -> std::convertible_to<bool>;
  { x.at(k) } -> std::convertible_to<value>;
  { x.insert(std::make_pair(k, k)) };
  { x.insert_or_assign(k, k) };
};


class match {
  public:
  match(value literals, value pattern)
  : m_literals {literals},
    m_pattern {pattern}
  { }

  template <value_mapping Mapping>
  bool
  operator () (value expr, Mapping &result) const
  { return _match(m_pattern, expr, result); }

  bool
  operator () (value expr) const
  {
    opi::unordered_map<value, value> _;
    return _match(m_pattern, expr, _);
  }

  private:
  // TODO: move in a different file
  template <value_mapping Mapping>
  bool
  _match(value pat, value expr, Mapping &result) const
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

      case tag::pair: {
        // Validate pattern-list
        if (issym(car(pat), "..."))
          throw std::runtime_error {std::format(
              "Ellipsis at the beginning of pattern-list: {}", pat)};

        value pit = pat, eit = expr;
        for (; pit->t == tag::pair; pit = cdr(pit))
        {
          if (pit->cdr->t == tag::pair and issym(car(cdr(pit)), "..."))
          // When followed by ellipsis, match as many consecutive elements of
          //  `expr` with the pattern as possible
          {
            opi::unordered_map<value, value> subresult;
            for (; eit->t == tag::pair and _match(car(pit), car(eit), subresult);
                 eit = cdr(eit))
            { // Append new matches
              for (const auto &[k, v] : subresult)
              {
                const value vallist = result.contains(k) ? result.at(k) : nil;
                result.insert_or_assign(k, append(vallist, cons(v, nil)));
              }
              subresult.clear();
            }
            // jump over ellipsis
            pit = cdr(pit);
          }
          else
          // Otherwize, match pattern-list element to expression-list element
          {
            if (eit->t != tag::pair)
              // Not enough elements in the expression
              return false;
            // Match elementas
            if (not _match(car(pit), car(eit), result))
              return false;
            eit = cdr(eit);
          }
        }

        // Finally, match list tail
        return _match(pit, eit, result);
      }

      default:
        return equal(pat, expr);
    }
  }

  private:
  value m_literals;
  value m_pattern;
};


} // namespace opi

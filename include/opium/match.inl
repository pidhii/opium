/**
 * \file match.inl
 * Template implementation of opium/match.hpp members 
 */
#pragma once

#include "opium/match.hpp"


template <opi::value_mapping Mapping>
bool
opi::match::_match(value pat, value expr, Mapping &result) const
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
          opi::stl::unordered_map<value, value> subresult;
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

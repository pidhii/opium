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
 * \file match.inl
 * Template implementation of opium/match.hpp members 
 */
#pragma once

#include "opium/match.hpp"
#include "opium/exceptions.hpp"


template <opi::value_mapping Mapping>
bool
opi::match::_match(value pat, value expr, Mapping &result) const
{
  switch (pat->t)
  {
    case tag::sym:
      if (sym_name(pat) == "_")
        return true;
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
        throw bad_code {std::format(
            "Ellipsis at the beginning of pattern-list: {}", pat), pat};

      value pit = pat, eit = expr;
      for (; pit->t == tag::pair; pit = cdr(pit))
      {
        if (cdr(pit)->t == tag::pair and issym(car(cdr(pit)), "..."))
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

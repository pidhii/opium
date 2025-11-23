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


#include "opium/code_transformer.hpp"
#include "opium/source_location.hpp"

using namespace std::placeholders;

/**
 * Implementation of the code_transformer class and its derivatives.
 * 
 * This file contains the implementation of the methods defined in code_transformer.hpp,
 * including rule management and expression transformation logic.
 */


void
opi::code_transformer::prepend_rule(const match &matcher,
                                    const transformation &transformer)
{ m_pages.front().emplace_front(matcher, transformer); }


void
opi::code_transformer::append_rule(const match &matcher,
                                   const transformation &transformer)
{ m_pages.front().emplace_back(matcher, transformer); }


opi::value
opi::code_transformer::operator () (value inexpr) const
{
  // Iterate through the syntax table and find the first matching rule
  match_mapping matches;
  for (const auto &[matcher, transformer] : m_pages | std::views::join)
  {
    if (matches.clear(), matcher(inexpr, matches))
    {
      const value result = transformer(matches, inexpr);
      copy_location(inexpr, result);
      return result;
    }
    matches.clear(); // Clean up after unsuccessful match
  }

  // If we reach here, no rule matched the input expression
  throw code_transformation_error {"no syntax rule matches the expression",
                                   inexpr};
}


opi::prolog_cleaner::prolog_cleaner()
{
  // FIXME
  append_rule({list("and"), cons("and", "clauses")}, [this](const auto &ms) {
    const value clauses = ms.at("clauses");

    value newclauses = nil;
    for (const value clause : range(clauses))
    {
      if (clause == True)
      {
        nop = false;
        continue;
      }
      else if (clause == False)
      {
        nop = false;
        return False;
      }
      append_mut(newclauses, list((*this)(clause)));
    }

    if (length(newclauses) == 0)
    {
      nop = false;
      return True;
    }
    else if (length(newclauses) == 1)
    {
      nop = false;
      return car(newclauses);
    }
    else
      return cons("and", newclauses);
  });

  append_rule({list("or"), cons("or", "clauses")}, [this](const auto &ms) {
    const value clauses = ms.at("clauses");

    value newclauses = nil;
    for (const value clause : range(clauses))
    {
      if (clause == False)
      {
        nop = false;
        continue;
      }
      append_mut(newclauses, list((*this)(clause)));
    }

    if (length(newclauses) == 0)
    {
      nop = false;
      return list(False);
    }
    if (length(newclauses) == 1)
    {
      nop = false;
      return car(newclauses);
    }
    else
      return cons("or", newclauses);
  });

  append_rule({list("if"), list("if", "cond", "then", "else")}, [this](const auto &ms) {
    const value newcond = (*this)(ms.at("cond"));
    const value newthenbr = (*this)(ms.at("then"));
    const value newelsebr = (*this)(ms.at("else"));
    if (newcond == True)
    {
      nop = false;
      return newthenbr;
    }
    else if (newcond == False)
    {
      nop = false;
      return newelsebr;
    }
    else
      return list("if", newcond, newthenbr, newelsebr);
  });

  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}
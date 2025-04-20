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


static void
_flatten_clauses(std::string_view tag, opi::value expr, opi::value &result)
{
  if (expr->t == opi::tag::pair and issym(car(expr), tag))
  {
    for (const opi::value clause : range(cdr(expr)))
      _flatten_clauses(tag, clause, result);
  }
  else
    result = append(result, list(expr));
}


opi::prolog_cleaner::prolog_cleaner()
{
  // FIXME
  // append_rule({list("and"), cons("and", "clauses")}, [this](const auto &ms) {
  //   value clauses = ms.at("clauses");

  //   // Flatten clauses w.r.t. nested AND statements
  //   value newclauses = nil;
  //   for (const value clause : range(clauses))
  //     _flatten_clauses("and", clause, newclauses);
  //   clauses = newclauses;

  //   if (length(clauses) == 1)
  //     return (*this)(car(clauses));
  //   else
  //     return  cons("and", clauses);
  // });

  // append_rule({list("or"), cons("or", "clauses")}, [this](const auto &ms) {
  //   value clauses = ms.at("clauses");

  //   // Flatten clauses w.r.t. nested OR statements
  //   value newclauses = nil;
  //   for (const value clause : range(clauses))
  //     _flatten_clauses("OR", clause, newclauses);
  //   clauses = newclauses;

  //   if (length(clauses) == 1)
  //     return (*this)(car(clauses));
  //   else
  //     return  cons("or", clauses);
  // });

  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}
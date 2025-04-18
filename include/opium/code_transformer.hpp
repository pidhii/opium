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

#include "opium/exceptions.hpp"
#include "opium/match.hpp"
#include "opium/stl/deque.hpp"
#include "opium/stl/list.hpp"
#include "opium/value.hpp"

#include <concepts>
#include <functional>

/**
 * \file code_transformer.hpp
 * Code transformation utilities for Lisp-style code
 * 
 * This file defines utilities for transforming Lisp-style symbolical-expression.
 * 
 * \ingroup lisp
 */


namespace opi {


// TODO: rename
template <typename T>
concept transformation = requires(const T t, value x)
{
  { t(x) } -> std::convertible_to<value>;
};

/**
 * Exception thrown when code transformation fails
 * 
 * \ingroup lisp
 */
struct code_transformation_error: public bad_code {
  using bad_code::bad_code;
}; // struct opi::code_transformation_error


/**
 * Generic transformer for S-expressions operating on a table of syntax-rules.
 * 
 * The code_transformer applies transformations to S-expressions based on pattern matching.
 * It maintains an ordered table of syntax rules, where each rule consists of a pattern matcher
 * and a transformation function. When transforming an expression, the transformer tries each
 * rule in order until it finds a match, then applies the corresponding transformation.
 * 
 * Rules can be added at the beginning (highest priority) or end (lowest priority) of the table.
 * 
 * Usage example:
 * ```
 * code_transformer transformer;
 * 
 * // Add a rule to transform (add x y) to (+ x y)
 * transformer.append_rule(
 *   match{list("add"), list("add", "x", "y")},
 *   [](const auto& ms) {
 *     return list("+", ms.at("x"), ms.at("y"));
 *   }
 * );
 * 
 * // Transform an expression
 * value result = transformer(list("add", num(1), num(2)));
 * // result is now (+ 1 2)
 * ```
 * 
 * \ingroup lisp
 */
class code_transformer {
  public:
  using match_mapping = opi::stl::unordered_map<value, value>;
  // TODO: implement std::function equivalent but with gc_allocator
  using transformation = std::function<value(const match_mapping&, value)>;
  using transformation_nofm = std::function<value(const match_mapping&)>;

  code_transformer() { m_pages.emplace_front(); }
  code_transformer(const code_transformer&) = delete;
  code_transformer(code_transformer&&) = delete;
  code_transformer& operator = (const code_transformer&) = delete;
  code_transformer& operator = (code_transformer&&) = delete;

  /**
   * Add syntax rule at highest priority (beginning of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param transformer The transformation function to apply when pattern matches
   */
  void
  prepend_rule(const match &matcher, const transformation &transformer);

  /**
   * Add syntax rule at highest priority (beginning of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param rule The transformation function to apply when pattern matches
   */
  template <typename RuleNoFM>
    requires std::regular_invocable<RuleNoFM, const match_mapping&>
  void
  prepend_rule(const match &matcher, RuleNoFM rule)
  { prepend_rule(matcher, [=](const auto &ms, value) { return rule(ms); }); }

  /**
   * Add syntax rule at lowest priority (end of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param transformer The transformation function to apply when pattern matches
   */
  void
  append_rule(const match &matcher, const transformation &transformer);

  /**
   * Add syntax rule at lowest priority (end of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param rule The transformation function to apply when pattern matches
   */
  template <typename RuleNoFM>
    requires std::regular_invocable<RuleNoFM, const match_mapping&>
  void
  append_rule(const match &matcher, RuleNoFM rule)
  { append_rule(matcher, [=](const auto &ms, value) { return rule(ms); }); }

  /**
   * Start new syntax table s.t. all rules inserted after calling these method
   * will have higher priority to the rules inserted prior to this point.
   */
  void
  flip_page()
  { m_pages.emplace_front(); }

  /**
   * Transform expression according to the rules in the syntax table.
   * 
   * The transformer tries each rule in order until it finds a match,
   * then applies the corresponding transformation. If no rule matches,
   * a code_transformation_error is thrown.
   * 
   * \param inexpr The expression to transform
   * \return The transformed expression
   * \throws code_transformation_error if no rule matches the input expression
   */
  value
  operator () (value inexpr) const;

  private:
  using syntax_table = opi::stl::deque<std::pair<match, transformation>>;
  opi::stl::list<syntax_table> m_pages; /**< Syntax tables */
}; // class opi::code_transformer
static_assert(transformation<code_transformer>);


template <transformation Lhs, transformation Rhs>
class composed_transformer {
  public:
  composed_transformer(const Lhs &lhs, const Rhs &rhs)
  : m_lhs {lhs}, m_rhs {rhs}
  { }

  value
  operator () (value inexpr) const
  { return m_lhs(m_rhs(inexpr)); }

  private:
  const Lhs &m_lhs;
  const Rhs &m_rhs;
}; // class opi::composed_transformer
static_assert(
    transformation<composed_transformer<code_transformer, code_transformer>>);


template <transformation Lhs, transformation Rhs>
composed_transformer<Lhs, Rhs>
compose(const Lhs &lhs, const Rhs &rhs)
{ return {lhs, rhs}; }


template <transformation Transformer>
value
transform_block(const Transformer &transformer, value block)
{ return list(range(block) | std::views::transform(std::ref(transformer))); }


// TODO: move to a separate file
struct prolog_cleaner: public code_transformer {
  prolog_cleaner();
}; // struct opi::prolog_cleaner

} // namespace opi

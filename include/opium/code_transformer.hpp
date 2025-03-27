#pragma once

#include "opium/match.hpp"
#include "opium/value.hpp"
#include "opium/stl/deque.hpp"
#include "opium/stl/list.hpp"

#include <concepts>
#include <functional>
#include <stdexcept>

/**
 * \file code_transformer.hpp
 * Code transformation utilities for Lisp-style code
 * 
 * This file defines utilities for transforming Lisp-style symbolical-expression.
 * 
 * \ingroup lisp
 */


namespace opi {


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
struct code_transformation_error: public std::runtime_error {
  using std::runtime_error::runtime_error;
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
  using transformation = std::function<value(const match_mapping&)>;

  code_transformer() { m_pages.emplace_front(); }

  /**
   * Add syntax rule at highest priority (beginning of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param transformer The transformation function to apply when pattern matches
   */
  void
  prepend_rule(const match &matcher, const transformation &transformer);

  /**
   * Add syntax rule at lowest priority (end of the syntax table).
   * 
   * \param matcher The pattern matcher that identifies expressions to transform
   * \param transformer The transformation function to apply when pattern matches
   */
  void
  append_rule(const match &matcher, const transformation &transformer);

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
  template <typename LhsArgs, typename RhsArgs>
    requires std::copy_constructible<Lhs> and std::copy_constructible<Rhs>
  composed_transformer(const std::tuple<LhsArgs> &lhsargs,
                       const std::tuple<RhsArgs> &rhsargs)
  : m_lhs {std::make_from_tuple<Lhs>(lhsargs)},
    m_rhs {std::make_from_tuple<Rhs>(rhsargs)}
  { }

  composed_transformer(const Lhs &lhs, const Rhs &rhs)
  : m_lhs {lhs}, m_rhs {rhs}
  { }

  composed_transformer(Lhs &&lhs, Rhs &&rhs)
  : m_lhs {std::move(lhs)}, m_rhs {std::move(rhs)}
  { }

  value
  operator () (value inexpr) const
  { return m_lhs(m_rhs(inexpr)); }

  private:
  Lhs m_lhs;
  Rhs m_rhs;
}; // class opi::composed_transformer
static_assert(
    transformation<composed_transformer<code_transformer, code_transformer>>);

template <transformation Lhs, transformation Rhs>
  requires std::copy_constructible<Lhs> and std::copy_constructible<Rhs>
composed_transformer<Lhs, Rhs>
compose(const Lhs &lhs, const Rhs &rhs)
{ return {lhs, rhs}; }


/**
 * A specialized code transformer with built-in support for Scheme syntax.
 *
 * This transformer includes rules for common Scheme constructs, propagating
 * transformations to all contained expressions. For example, in an if-expression,
 * the condition, then-branch, and else-branch are all recursively transformed.
 *
 * Supported Scheme syntax:
 * - `(if <cond> <then> <else>)`: Conditional expression
 *   - Transforms to: `(if T[<cond>] T[<then>] T[<else>])`
 *
 * - `(let ((<ident> <expr>) ...) body ...)`: Local variable binding
 *   - Transforms to: `(let ((<ident> T[<expr>]) ...) T[body] ...)`
 *
 * - `(let* ((<ident> <expr>) ...) body ...)`: Sequential local variable binding
 *   - Transforms to: `(let* ((<ident> T[<expr>]) ...) T[body] ...)`
 *
 * - `(letrec ((<ident> <expr>) ...) body ...)`: Recursive local variable binding
 *   - Transforms to: `(letrec ((<ident> T[<expr>]) ...) T[body] ...)`
 *
 * - `(letrec* ((<ident> <expr>) ...) body ...)`: Sequential recursive local variable binding
 *   - Transforms to: `(letrec* ((<ident> T[<expr>]) ...) T[body] ...)`
 *
 * - `(let-values (((<ident> ...) <expr>) ...) body ...)`: Multiple value binding
 *   - Transforms to: `(let-values (((<ident> ...) T[<expr>]) ...) T[body] ...)`
 *
 * - `(let*-values (((<ident> ...) <expr>) ...) body ...)`: Sequential multiple value binding
 *   - Transforms to: `(let*-values (((<ident> ...) T[<expr>]) ...) T[body] ...)`
 *
 * - `(define <ident> <body> ...)`: Variable or function definition
 *   - Transforms to: `(define <ident> T[<body>] ...)`
 *
 * \ingroup lisp
 */
struct scheme_code_transformer: public code_transformer {
  /**
   * Constructs a code transformer with built-in Scheme syntax rules.
   *
   * Initializes the syntax table with rules for transforming common Scheme
   * constructs like if, let, let*, letrec, letrec*, let-values, let*-values, and define.
   * Each rule recursively applies the transformer to all contained expressions.
   */
  scheme_code_transformer();
}; // struct opi::scheme_code_transformer
static_assert(transformation<code_transformer>);


} // namespace opi

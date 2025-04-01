#pragma once

#include "opium/code_transformer.hpp"

/**
 * \file scheme_code_transformer.hpp
 * Scheme-specific code transformation utilities
 * 
 * This file defines a specialized code transformer with built-in support for Scheme syntax.
 * 
 * \ingroup lisp
 */

namespace opi {

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
 * - `(lambda <args> <body> ...)`: Lambda expression
 *   - Transforms to: `(lambda <args> T[<body>] ...)`
 *
 * - `(begin <body> ...)`: Sequence of expressions
 *   - Transforms to: `(begin T[<body>] ...)`
 *
 * - `(quote <x> ...)`: Quoted expression
 *   - Transforms to: `(quote <x> ...)` (no transformation applied to quoted content)
 *
 * \ingroup lisp
 */
struct scheme_code_transformer: public code_transformer {
  /**
   * Constructs a code transformer with built-in Scheme syntax rules.
   *
   * Initializes the syntax table with rules for transforming common Scheme
   * constructs like if, let, let*, letrec, letrec*, let-values, let*-values, define,
   * lambda, begin, and quote. Each rule recursively applies the transformer to all 
   * contained expressions.
   */
  scheme_code_transformer();
}; // struct opi::scheme_code_transformer
static_assert(transformation<scheme_code_transformer>);

} // namespace opi

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


#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/utilities/ranges.hpp"
#include "opium/logging.hpp"

#include <functional>

using namespace std::placeholders;

/**
 * Implementation of the scheme_code_transformer class.
 * 
 * This file contains the implementation of the scheme_code_transformer class,
 * which provides built-in rules for transforming common Scheme constructs.
 */

opi::scheme_code_transformer::scheme_code_transformer()
{
  /**
   * Rule for if-statement (T-agnostic scheme syntax)
   * 
   * Propagates transformation to all contained expressions:
   * (if <cond-expr> <then-expr> <else-expr>) ->
   * (if T[<cond-expr>] T[<then-expr>] T[<else-expr>])
   */
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  append_rule(ifmatch, [this](const auto &ms) {
    return list("if", (*this)(ms.at("cond")),
                      (*this)(ms.at("then")),
                      (*this)(ms.at("else")));
  });

  /**
   * Rule for else-less if-statement (T-agnostic scheme syntax)
   * 
   * Propagates transformation to all contained expressions:
   * (if <cond-expr> <then-expr>) ->
   * (if T[<cond-expr>] T[<then-expr>])
   */
  const match ifnoelsematch {list("if"), list("if", "cond", "then")};
  append_rule(ifnoelsematch, [this](const auto &ms) {
    return list("if", (*this)(ms.at("cond")),
                      (*this)(ms.at("then")));
  });
  /**
   * Rules for let-family bindings (T-agnostic scheme syntax)
   * 
   * Propagates transformation to all contained expressions:
   * (let ((<bind-id> <bind-expr>)
   *            ...               )
   *   <body-expr>
   *       ...    ) ->
   * (let ((<bind-id> T[<bind-expr>])
   *              ...                )
   *   T[<body-expr>]
   *       ...       )
   */
  // Define the common pattern for all let-family expressions
  const value let_pattern =
      list(list(list("ident", "expr"), "..."), "body", "...");

  // Helper function to transform let-family expressions
  auto let_rule = [this](const std::string &let, const auto &ms) {
    value idents = ms.contains("ident") ? ms.at("ident") : nil;
    value exprs = ms.contains("expr") ? ms.at("expr") : nil;
    const value body = ms.contains("body") ? ms.at("body") : nil;
    value newbinds = nil;
    for (; ispair(exprs); idents = cdr(idents), exprs = cdr(exprs))
      newbinds = append(newbinds, list(list(car(idents), (*this)(car(exprs)))));
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return list(sym(let), newbinds, dot, newbody);
  };

  // Add rules for each let-family variant
  // let - basic local variable binding
  append_rule(match {list("let"), cons("let", let_pattern)},
              std::bind(let_rule, "let", _1));
  // let* - sequential local variable binding
  append_rule(match {list("let*"), cons("let*", let_pattern)},
              std::bind(let_rule, "let*", _1));
  // letrec - recursive local variable binding
  append_rule(match {list("letrec"), cons("letrec", let_pattern)},
              std::bind(let_rule, "letrec", _1));
  // letrec* - sequential recursive local variable binding
  append_rule(match {list("letrec*"), cons("letrec*", let_pattern)},
              std::bind(let_rule, "letrec*", _1));
  // let-values - multiple value binding
  append_rule(match {list("let-values"), cons("let-values", let_pattern)},
              std::bind(let_rule, "let-values", _1));
  // let*-values - sequential multiple value binding
  append_rule(match {list("let*-values"), cons("let*-values", let_pattern)},
              std::bind(let_rule, "let*-values", _1));

  /**
   * Rule for define statements (T-agnostic scheme syntax)
   * 
   * Propagates transformation to all contained expressions:
   * (define <ident> <body> ...) ->
   * (define <ident> T[<body>] ...)
   * 
   * This ensures that all expressions within a define statement
   * are also transformed according to the rules.
   */
  const match definematch {list("define"), list("define", "ident", dot, "body")};
  append_rule(definematch, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.at("body");
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return list("define", ident, dot, newbody);
  });

  const match defvalsmatch {list("define-values"),
                            list("define-values", "idents", dot, "body")};
  append_rule(defvalsmatch, [this](const auto &ms) {
    const value idents = ms.at("idents");
    const value body = ms.at("body");
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return list("define-values", idents, dot, newbody);
  });


  /**
   * Rule for lambda expressions
   * 
   * Propagates transformation to all contained expressions:
   * (lambda <args> <body> ...) ->
   * (lambda <args> T[<body>] ...)
   */
  const match lambdamatch {list("lambda"), list("lambda", "args", dot, "body")};
  append_rule(lambdamatch, [this](const auto &ms) {
    const value args = ms.at("args");
    const value body = ms.at("body");
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return list("lambda", args, dot, newbody);
  });

  /**
   * Rule for begin expressions
   * 
   * Propagates transformation to all contained expressions:
   * (begin <body> ...) ->
   * (begin T[<body>] ...)
   */
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  append_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return cons("begin", newbody);
  });

  /**
   * Rule for and expressions
   *
   * Propagates transformation to all contained expressions:
   * (and <clause> ...) ->
   * (and T[<clause>] ...)
   */
  const match andmatch {list("and"), list("and", dot, "clauses")};
  append_rule(andmatch, [this](const auto &ms) {
    const value clauses = ms.at("clauses");
    const value newclauses =
        list(range(clauses) | std::views::transform(std::ref(*this)));
    return cons("and", newclauses);
  });

  /**
   * Rule for or expressions
   *
   * Propagates transformation to all contained expressions:
   * (or <clause> ...) ->
   * (or T[<clause>] ...)
   */
  const match ormatch {list("or"), list("or", dot, "clauses")};
  append_rule(ormatch, [this](const auto &ms) {
   const value clauses = ms.at("clauses");
   const value newclauses =
       list(range(clauses) | std::views::transform(std::ref(*this)));
   return cons("or", newclauses);
  });

  /**
   * Rule for set! expressions
   */
  const match setmatch {list("set!"), list("set!", "ident", "expr")};
  append_rule(setmatch, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value expr = ms.at("expr");
    return list("set!", (*this)(ident), (*this)(expr));
  });

  /**
   * Rule for quote expressions
   * 
   * Preserves quoted content without transformation:
   * (quote <x> ...) ->
   * (quote <x> ...)
   */
  const match quotematch {list("quote"), list("quote", dot, "x")};
  append_rule(quotematch, [](const auto &ms) {
    return list("quote", dot, ms.at("x"));
  });
}


opi::ext_scheme_code_transformer::ext_scheme_code_transformer()
{
  /**
   * Rule for define statements (T-agnostic extended-scheme syntax)
   * 
   * Propagates transformation to all contained expressions:
   * (define-overload <ident> <body> ...) ->
   * (define-overload <ident> T[<body>] ...)
   */
  const match defineovlmatch {list("define-overload"),
                              list("define-overload", "ident", dot, "body")};
  prepend_rule(defineovlmatch, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.at("body");
    const value newbody = list(range(body) | std::views::transform(std::ref(*this)));
    return list("define-overload", ident, dot, newbody);
  });

  // TODO: fixme
  const match casesmatch {
      list("cases"), list("cases", "exprs", cons("patterns", "branch"), "...")};
  prepend_rule(casesmatch, [this](const auto &ms) {
    const value exprs = ms.at("exprs");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;
    warning("branches: {}", branches);
    
    const value newexprs = transform_block(*this, exprs);

    value newcases = nil;
    for (const auto &[rowpatterns, branch] :
         utl::zip(range(patterns), range(branches)))
    {
      const value newbranch = transform_block(*this, branch);
      const value newcase = cons(patterns, newbranch);
      newcases = append(newcases, list(newcase));
    }

    warning("new cases: {}", newcases);
    return list("cases", newexprs, dot, newcases);
  });

  /** 
   * pragma
   *
   * Pass as is without modifications. This results in all transformers that are
   * not explicitly handle this case to, effectively, ignore it, without applying
   * any permutations; and only those transformers that explicitly overwrite this
   * rule will affect the propagation of such expression.
   */
  const match tchmatch {list("pragma"), list("pragma", dot, "_")};
  prepend_rule(tchmatch, [](const auto &, value fm) { return fm; });
}
#include "opium/scheme/scheme_code_transformer.hpp"

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
   * 
   * This ensures that all subexpressions within an if statement
   * are also transformed according to the rules.
   */
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  append_rule(ifmatch, [this](const auto &ms) {
    return list("if", (*this)(ms.at("cond")),
                      (*this)(ms.at("then")),
                      (*this)(ms.at("else")));
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
   * 
   * This pattern applies to all variants: let, let*, letrec, letrec*,
   * let-values, and let*-values. The implementation uses a common helper
   * function (let_rule) that handles all these variants.
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
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
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

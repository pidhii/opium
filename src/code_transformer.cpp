#include "opium/code_transformer.hpp"

#include <format>
#include <functional>

/**
 * Implementation of the code_transformer class and its derivatives.
 * 
 * This file contains the implementation of the methods defined in code_transformer.hpp,
 * including rule management and expression transformation logic.
 */


void
opi::code_transformer::prepend_rule(const match &matcher,
                                    const transformation &transformer)
{ m_syntax_table.emplace_front(matcher, transformer); }


void
opi::code_transformer::append_rule(const match &matcher,
                                   const transformation &transformer)
{ m_syntax_table.emplace_back(matcher, transformer); }


opi::value
opi::code_transformer::operator () (value inexpr) const
{
  // Iterate through the syntax table and find the first matching rule
  match_mapping matches;
  for (const auto &[matcher, transformer]: m_syntax_table)
  {
    if (matcher(inexpr, matches))
      return transformer(matches);
    matches.clear(); // Clean up after unsuccessful match
  }

  // If we reach here, no rule matched the input expression
  throw code_transformation_error {
      std::format("no syntax rule matches the expression: {}", inexpr)};
}


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
  append_rule(
    match {
      // literals:
      list("if"),
      // pattern:
      list("if", "cond", "then", "else")
    },
    [this](const auto &ms) {
      return list("if", (*this)(ms.at("cond")),
                        (*this)(ms.at("then")),
                        (*this)(ms.at("else")));
    }
  );

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
    value newbody = nil;
    for (const value x : range(body))
      newbody = append(newbody, list((*this)(x)));
    return list(sym(let), newbinds, dot, newbody);
  };

  // Add rules for each let-family variant
  // let - basic local variable binding
  append_rule(match {list("let"), cons("let", let_pattern)},
              std::bind(let_rule, "let", std::placeholders::_1));
  // let* - sequential local variable binding
  append_rule(match {list("let*"), cons("let*", let_pattern)},
              std::bind(let_rule, "let*", std::placeholders::_1));
  // letrec - recursive local variable binding
  append_rule(match {list("letrec"), cons("letrec", let_pattern)},
              std::bind(let_rule, "letrec", std::placeholders::_1));
  // letrec* - sequential recursive local variable binding
  append_rule(match {list("letrec*"), cons("letrec*", let_pattern)},
              std::bind(let_rule, "letrec*", std::placeholders::_1));
  // let-values - multiple value binding
  append_rule(match {list("let-values"), cons("let-values", let_pattern)},
              std::bind(let_rule, "let-values", std::placeholders::_1));
  // let*-values - sequential multiple value binding
  append_rule(match {list("let*-values"), cons("let*-values", let_pattern)},
              std::bind(let_rule, "let*-values", std::placeholders::_1));
}

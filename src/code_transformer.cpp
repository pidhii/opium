#include "opium/code_transformer.hpp"

#include <format>
#include <functional>


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
  // Find matching syntax and apply corresponding transformation
  match_mapping matches;
  for (const auto &[matcher, transformer]: m_syntax_table)
  {
    if (matcher(inexpr, matches))
      return transformer(matches);
    matches.clear(); // Clean up after unsuccessful match
  }

  // No rule matches the input expression
  throw code_transformation_error {
      std::format("no syntax rule matches the expression: {}", inexpr)};
}


opi::scheme_code_transformer::scheme_code_transformer()
{
  // if-statement (T-agnostic scheme syntax)
  // ---------------------------------------
  // Propagate transformation to all contained expressions:
  //
  // (if <cond-expr> <then-expr> <else-expr>) ->
  // (if T[<cond-expr>] T[<then-expr>] T[<else-expr>])
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

  // let[rec][*][-values]-bindings (T-agnostic scheme syntax)
  // ---------------------------------------
  // Propagate transformation all contained expressions:
  //
  // (let ((<bind-id> <bind-expr>)
  //            ...               )
  //   <body-expr>
  //       ...    ) ->
  // (let ((<bind-id> T[<bind-expr>])
  //              ...                )
  //   T[<body-expr>]
  //       ...       )
  const value let_pattern =
      list(list(list("ident", "expr"), "..."), "body", "...");
  auto let_rule = [this](const std::string &let, const auto &ms) {
    value idents = ms.contains("ident") ? ms.at("ident") : nil;
    value exprs = ms.contains("expr") ? ms.at("expr") : nil;
    const value body = ms.at("body");
    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
      newbinds = append(newbinds, list(list(car(idents), (*this)(car(exprs)))));
    value newbody = nil;
    for (const value x : range(body))
      newbody = append(newbody, list((*this)(x)));
    return list(sym(let), newbinds, dot, newbody);
  };
  // let
  append_rule(match {list("let"), cons("let", let_pattern)},
              std::bind(let_rule, "let", std::placeholders::_1));
  // let*
  append_rule(match {list("let*"), cons("let*", let_pattern)},
              std::bind(let_rule, "let*", std::placeholders::_1));
  // letrec
  append_rule(match {list("letrec"), cons("letrec", let_pattern)},
              std::bind(let_rule, "letrec", std::placeholders::_1));
  // letrec*
  append_rule(match {list("letrec*"), cons("letrec*", let_pattern)},
              std::bind(let_rule, "letrec*", std::placeholders::_1));
  // let-values
  append_rule(match {list("let-values"), cons("let-values", let_pattern)},
              std::bind(let_rule, "let-values", std::placeholders::_1));
  // let*-values
  append_rule(match {list("let*-values"), cons("let*-values", let_pattern)},
              std::bind(let_rule, "let*-values", std::placeholders::_1));
}

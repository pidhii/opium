#include "opium/pretty_print.hpp"
#include "opium/code_transformer.hpp"
#include "opium/value.hpp"


opi::pretty_printer::pretty_printer(const code_transformer &formatter)
: m_formatter {formatter}
{ }


void
opi::pretty_printer::print(std::ostream &os, value x, int indent)
{
  x = m_formatter(x);

  const match blockfmtpat {
      list("_FormatBlock"),
      list("_FormatBlock", "keep-first", "extra-indent", "expr")};
  opi::stl::unordered_map<value, value> matches;
  if (blockfmtpat(x, matches))
  {
    _block_format fmt;
    fmt.keep_first = matches.at("keep-first") != False;
    fmt.extra_indent = num_val(matches.at("extra-indent"));
    _print_block(os, matches.at("expr"), indent, fmt);
  }
  else
    os << x;
}


static void
_print_indent(std::ostream &os, int level)
{
  for (int i = 0; i < level; ++i)
    os << " "; // Two spaces per indent level
};


void
opi::pretty_printer::_print_block(std::ostream &os, opi::value stmt, int indent,
                                  const _block_format &fmt)
{
  using namespace opi;

  if (stmt->t != tag::pair)
    return print(os, stmt, indent);

  const std::string op = car(stmt)->sym.data;
  const value clauses = cdr(stmt);

  // Print the operator
  os << "(" << car(stmt);

  // If there are no clauses, just close the parenthesis
  if (clauses->t != tag::pair)
  {
    os << ")";
    return;
  }

  // Print first clause
  if (fmt.keep_first)
    os << " ";
  else
    os << "\n", _print_indent(os, indent + fmt.extra_indent);
  print(os, car(clauses), indent + fmt.extra_indent);

  // Print each subsequent clause on a new line with increased indentation
  for (value clause : range(cdr(clauses)))
  {
    os << std::endl;
    _print_indent(os, indent + fmt.extra_indent);
    print(os, clause, indent + fmt.extra_indent);
  }

  // Close the parenthesis
  os << ")";
}


opi::scheme_formatter::scheme_formatter()
{
  append_rule(match {list("if"), list("if", "cond", "then", "else")},
              [&](const auto &ms) {
                const value expr =
                    list("if", ms.at("cond"), ms.at("then"), ms.at("else"));
                return pretty_printer::format_block(true, 4, expr);
              });

  const value let_pat = list(list(list("ident", "expr"), "..."), "body", "...");
  auto let_rule = [&](const std::string &let, const auto &ms) {
    value idents = ms.contains("ident") ? ms.at("ident") : nil;
    value exprs = ms.contains("expr") ? ms.at("expr") : nil;
    const value body = ms.contains("body") ? ms.at("body") : nil;
    value binds = nil;
    for (; idents->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
      binds = append(binds, list(list(car(idents), car(exprs))));
    binds = pretty_printer::format_block(false, 1 + let.length(), binds);
    const value letexpr = list(sym(let), binds, dot, body);
    return pretty_printer::format_block(true, 2, letexpr);
  };
  append_rule(match {list("let"), cons("let", let_pat)},
              std::bind(let_rule, "let", std::placeholders::_1));
  append_rule(match {list("let*"), cons("let*", let_pat)},
              std::bind(let_rule, "let*", std::placeholders::_1));
  append_rule(match {list("letrec"), cons("letrec", let_pat)},
              std::bind(let_rule, "letrec", std::placeholders::_1));
  append_rule(match {list("let-values"), cons("let-values", let_pat)},
              std::bind(let_rule, "let-values", std::placeholders::_1));
  append_rule(match {list("let*-values"), cons("let*-values", let_pat)},
              std::bind(let_rule, "let*-values", std::placeholders::_1));

  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}


opi::prolog_formatter::prolog_formatter()
{
  append_rule({list("and"), cons("and", "clauses")}, [this](const auto &ms) {
    value clauses = ms.at("clauses");
    // TODO: Move this cleanup stuff to a separate transformer
    while (length(clauses) == 1 and issym(car(car(clauses)), "and"))
      clauses = cdr(car(clauses));
    if (length(clauses) == 1)
      return (*this)(car(clauses));
    else
      return pretty_printer::format_block(true, 5, cons("and", clauses));
  });

  append_rule({list("or"), cons("or", "clauses")}, [](const auto &ms) {
    const value clauses = ms.at("clauses");
    return pretty_printer::format_block(true, 4, cons("or", clauses));
  });

  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}
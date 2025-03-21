#include "opium/query.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"

#include <iostream>


/**
 * Helper function to pretty print predicate body with indentation
 * 
 * \param os Output stream to write to
 * \param body Predicate body to print
 * \param indent_level Current indentation level
 */
static void
_pretty_print_body(std::ostream &os, opi::value body, int indent_level = 0)
{
  using namespace opi;

  /**
   * Helper lambda for indentation
   */
  auto print_indent = [&os](int level) {
    for (int i = 0; i < level; ++i)
      os << " "; // Two spaces per indent level
  };

  // Handle different types of expressions
  if (body->t == tag::pair)
  {
    // Handle special operators and predicates
    if (issym(car(body)))
    {
      const std::string op = car(body)->sym.data;
      const value clauses = cdr(body);

      // Special handling for 'and' and 'or' operators
      if (op == "and" || op == "or")
      {
        // Print the operator
        os << "(" << op;

        // If there are no clauses, just close the parenthesis
        if (clauses->t != tag::pair)
          os << ")";

        // Print first clause on the same line
        os << " ";
        _pretty_print_body(os, car(clauses), indent_level + 2 + op.length());

        // Print each subsequent clause on a new line with increased indentation
        for (value clause : range(cdr(clauses)))
        {
          os << std::endl;
          print_indent(indent_level + 2 + op.length());
          _pretty_print_body(os, clause, indent_level + 2 + op.length());
        }

        // Close the parenthesis
        os << ")";
      }
      // Handle predicate calls with multiple arguments
      else if (length(clauses) > 1)
      {
        os << "(" << op;

        // Print first argument
        os << " ";
        _pretty_print_body(os, car(clauses), indent_level);

        // Print remaining arguments with proper indentation
        value rest = cdr(clauses);
        while (rest->t == tag::pair)
        {
          os << " ";
          _pretty_print_body(os, car(rest), indent_level);
          rest = cdr(rest);
        }

        os << ")";
      }
      // Simple predicate call or other expression
      else
      {
        os << "(" << op;

        // Print arguments
        for (value arg : range(clauses))
        {
          os << " ";
          _pretty_print_body(os, arg, indent_level);
        }

        os << ")";
      }
    }
    // Non-symbol car, use default printing
    else
    {
      os << "(";
      _pretty_print_body(os, car(body), indent_level);

      value rest = cdr(body);
      while (rest->t == tag::pair)
      {
        os << " ";
        _pretty_print_body(os, car(rest), indent_level);
        rest = cdr(rest);
      }

      if (rest->t != tag::nil)
      {
        os << " . ";
        _pretty_print_body(os, rest, indent_level);
      }

      os << ")";
    }
  }
  else
    // For other types, use the default printing
    os << body;
}


void
opi::prolog_repl::operator << (opi::value expr)
{
  // Handle a new predicate definition
  if (issym(car(expr), "predicate"))
  {
    const value signature = car(cdr(expr));
    value body = cdr(cdr(expr));
    switch (length(body))
    {
      case 0:
        body = True;
        break;
      case 1:
        body = car(body);
        break;
      default:
        body = cons(sym("and"), body);
        break;
    }
    const predicate &pred = add_predicate(signature, body);
    std::cout << pred.name() << opi::list(pred.arguments()) << " :- ";
    if (length(pred.body()) > 1)
    {
      std::cout << "\n  ";
      _pretty_print_body(std::cout, pred.body(), 2);
    }
    else
      _pretty_print_body(std::cout, pred.body());
    std::cout << "\n" << std::endl;
    return;
  }

  // Handle a query expression
  if (issym(car(expr), "query"))
  {
    _query(car(cdr(expr)));
    std::cout << std::endl;
    return;
  }

  throw error {std::format("Don't understand expression: {}", expr)};
}



void
opi::prolog_repl::_query(opi::value expr)
{
  std::cout << "?- ";
  _pretty_print_body(std::cout, expr, 3);
  std::cout << std::endl;

  /**
   * Run query over `expr`
   */
  predicate_runtime prt;
  unified_determined_summary summary {prt};
  make_true(prt, insert_cells(prt, expr), std::ref(summary),
            assign_nonterminal_to(sym("<nonterminal>")));

  for (const auto &[var, vals] : summary)
  {
    std::cout << var << " = ";
    for (std::string prefix = ""; const value &val : vals)
      std::cout << prefix << val, prefix = " | ";
    std::cout << std::endl;
  }
  std::cout << (summary.empty() ? "=> no" : "=> yes") << std::endl;
}

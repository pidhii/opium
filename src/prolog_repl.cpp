#include "opium/prolog_repl.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"
#include <iostream>
#include <sstream>


// Helper function to pretty print predicate body with indentation
static void
_pretty_print_body(std::ostream &os, opi::value body, int indent_level = 0)
{
  using namespace opi;

  // Helper for indentation
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
  // New predicate
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

  // Query
  if (issym(car(expr), "query"))
  {
    _query(car(cdr(expr)));
    std::cout << std::endl;
    return;
  }

  throw error {format("Don't understand expression: ", expr)};
}



void
opi::prolog_repl::_query(opi::value expr)
{
  std::cout << "?- ";
  _pretty_print_body(std::cout, expr, 3);
  std::cout << std::endl;

  // Run query over `expr`
  bool success = false;
  predicate_runtime prt;
  make_true(prt, insert_cells(prt, expr), [&]() {
    success = true;
    std::vector<std::string> fragms;
    for (const value var : prt.variables())
      fragms.emplace_back(format(var, " = ", reconstruct(prt[var])));

    if (not fragms.empty())
    {
      for (std::string prefix = ""; const std::string &s : fragms)
        std::cout << prefix << s, prefix = ", ";
      std::cout << "." << std::endl;
    }
  });

  std::cout << (success ? "=> yes" : "=> no") << std::endl;
}

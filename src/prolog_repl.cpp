#include "opium/query.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"
#include "opium/pretty_print.hpp"
#include "opium/lisp_parser.hpp"

#include <iostream>
#include <fstream>


void
opi::prolog_repl::operator << (opi::value expr)
{
  const prolog_formatter plfmt;
  pretty_printer pprint {plfmt};

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
        copy_location(expr, body);
        break;
    }
    const predicate &pred = add_predicate(signature, body);
    std::cout << std::format("{}{} :- ", pred.name(),
                             opi::list(pred.arguments()));
    if (length(pred.body()) > 1)
    {
      std::cout << "\n  ";
      pprint(std::cout, pred.body(), 2);
    }
    else
      pprint(std::cout, pred.body());
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

  // Handle imports
  if (issym(car(expr), "ensure-loaded"))
  {
    namespace fs = std::filesystem;
    const value x = car(cdr(expr));
    const fs::path fullpath = fs::absolute(str_view(x));

    if (not m_loaded_pathes.emplace(fullpath).second)
      // File already loaded, thus do nothing
      return;

    // Open the file
    if (std::ifstream infile {fullpath, std::ios::binary})
    {
      // Parse and process all expressions within the file
      lisp_parser parser;
      const auto tokens = parser.tokenize(infile, fullpath);
      size_t cursor = 0;
      while (cursor < tokens.size())
        (*this) << parser.parse_tokens(tokens, cursor);
    }
    else
    {
      throw error {std::format("Failed to open file required by: {}", expr)};
    }

    return;
  }

  throw error {std::format("Don't understand expression: {}", expr)};
}



void
opi::prolog_repl::_query(opi::value expr)
{
  const prolog_formatter plfmt;
  pretty_printer pprint {plfmt};

  std::cout << "?- ";
  pprint(std::cout, expr, 3);
  std::cout << std::endl;

  /**
   * Run query over `expr`
   */
  predicate_runtime prt;
  unified_determined_summary summary {prt};
  stl::unordered_set<cell*> nonterminals;
  make_true(prt, insert_cells(prt, expr), std::ref(summary),
            [&nonterminals](const auto &, cell *x) {
              nonterminals.insert(x);
              return cons("__cell", ptr(x));
            });

  for (const auto &[var, vals] : summary)
  {
    std::cout << var << " = ";
    const std::string prefix_after =
        "\n" + std::string(sym_name(var).length() + 1, ' ') + "| ";
    for (std::string prefix = ""; const value &val : vals)
      std::cout << prefix << val, prefix = prefix_after;
    std::cout << std::endl;
  }
  std::cout << (summary ? "=> yes" : "=> no") << std::endl;
}

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


#include "opium/query.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"
#include "opium/pretty_print.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/utilities/path_resolver.hpp"

#include <iostream>
#include <fstream>


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
        copy_location(expr, body);
        break;
    }
    add_predicate(signature, body);
    return;
  }

  // Handle a query expression
  if (issym(car(expr), "query"))
  {
    _query(car(cdr(expr)));
    return;
  }

  // Handle imports
  if (issym(car(expr), "ensure-loaded"))
  {
    namespace fs = std::filesystem;
    const value x = car(cdr(expr));
    const fs::path fullpath = resolve_path(str_view(x), m_path_prefixes.begin(),
                                           m_path_prefixes.end());

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
  std::cout << "?- ";
  pprint_pl(std::cout, expr, 3);
  std::cout << std::endl;

  /**
   * Run query over `expr`
   */
  predicate_runtime prt;
  unified_determined_summary summary {prt};
  stl::unordered_set<cell*> nonterminals;
  make_true(insert_cells(prt, expr), std::ref(summary),
            [&nonterminals](const auto &, cell *x) {
              nonterminals.insert(x);
              return cons(cell_tag, ptr(x));
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

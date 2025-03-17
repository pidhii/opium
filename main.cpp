#include "opium/value.hpp"
#include "opium/format.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/logging.hpp"
#include "opium/prolog.hpp"

#include <cassert>
#include <cctype>
#include <concepts>
#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <ranges>
#include <utility>
#include <vector>

int
main([[maybe_unused]] int argc, char **argv)
{
  using namespace opi;

  std::ifstream infile{argv[1], std::ios::binary};

  prolog pl;
  lisp_parser parser;
  opi::vector<value> queries;

  const auto tokens = parser.tokenize(infile);
  size_t cursor = 0;
  while (cursor < tokens.size())
  {
    const value expr = parser.parse_tokens(tokens, cursor);
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
      pl.add_predicate(signature, body);
    }
    else if (issym(car(expr), "query"))
      queries.push_back(car(cdr(expr)));
    else
      throw std::runtime_error{format("undefined expression: ", expr)};
  }

  for (const value query : queries)
  {
    predicate_runtime prt;
    std::cout << "?- " << query << std::endl;
    pl.make_true(prt, query, [&](const predicate_runtime &result_prt) {
      std::cout << "=> yes" << std::endl;
      for (const value var : result_prt.variables())
      {
        value val = nil;
        if (result_prt.get_value(var, val))
          std::cout << " " << var << " = " << val << std::endl;
        else
          std::cout << " " << var << " = ?" << std::endl;
      }
    });
  }
}

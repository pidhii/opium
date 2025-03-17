#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/logging.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/stl/vector.hpp"
#include "opium/value.hpp"
#include "opium/format.hpp"

#include <cassert>
#include <cctype>
#include <fstream>
#include <ios>
#include <iostream>
#include <vector>


int
main([[maybe_unused]] int argc, char **argv)
{
  using namespace opi;

  prolog_repl pl;
  lisp_parser parser;
  opi::vector<value> queries;

  // Read input file
  std::ifstream infile{argv[1], std::ios::binary};
  const auto tokens = parser.tokenize(infile);
  size_t cursor = 0;
  while (cursor < tokens.size())
    pl << parser.parse_tokens(tokens, cursor);

  // Helper function to read a line with prompt
  auto prompt_line = [&](const std::string &prompt, std::string &line) -> bool {
    std::cout << prompt;
    std::cout.flush();
    return bool(std::getline(std::cin, line));
  };

  std::cout << "\n"; // Empty line before the REPL

  // Run REPL
  lisp_reader reader {parser};
  for (std::string line; prompt_line("> ", line); line.clear())
  {
    // Feed new piece of text into the reader
    reader << line;

    // Process new expressions
    value expr = nil;
    while (reader >> expr)
    {
      try { pl << expr; }
      catch (const opi::prolog_repl::error &exn)
      { std::cout << exn.what() << std::endl; }
    }
  }
}

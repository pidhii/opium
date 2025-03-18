#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"

#include <cassert>
#include <cctype>
#include <fstream>
#include <ios>
#include <iostream>
#include <vector>


// Helper function to read a line with prompt
static bool
prompt_line(const std::string &prompt, std::string &line)
{
  std::cout << prompt;
  std::cout.flush();
  return bool(std::getline(std::cin, line));
}


int
main([[maybe_unused]] int argc, char **argv)
{
  using namespace opi;

  prolog_repl pl;
  lisp_parser parser;

  // Read input file
  std::ifstream infile {argv[1], std::ios::binary};
  const auto tokens = parser.tokenize(infile);
  size_t cursor = 0;
  while (cursor < tokens.size())
    pl << parser.parse_tokens(tokens, cursor);

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

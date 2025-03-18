#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"

#include <boost/program_options.hpp>

#include <cassert>
#include <cctype>
#include <cstdlib>
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
main(int argc, char **argv)
{
  namespace po = boost::program_options;
  using namespace opi;

  // Define command line options
  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::string>(), "input file to process");

  po::positional_options_description posdesc;
  posdesc.add("input-file", 1);

  po::variables_map varmap;
  try
  {
    auto parsedopts = po::command_line_parser(argc, argv)
                          .options(desc)
                          .positional(posdesc)
                          .run();
    po::store(parsedopts, varmap);
    po::notify(varmap);
  }
  catch (const po::error &e)
  {
    error(e.what());
    std::cerr << desc << std::endl;
    return 1;
  }

  if (varmap.count("help"))
  {
    std::cout << "Usage: " << argv[0] << " [options] [input-file]" << std::endl;
    std::cout << desc << std::endl;
    return 0;
  }

  prolog_repl pl;
  lisp_parser parser;

  // Read input file if provided
  if (varmap.count("input-file"))
  {
    const std::string input_file = varmap["input-file"].as<std::string>();
    if (std::ifstream infile {input_file, std::ios::binary})
    {
      const auto tokens = parser.tokenize(infile);
      size_t cursor = 0;
      while (cursor < tokens.size())
        pl << parser.parse_tokens(tokens, cursor);

      // Empty line before the REPL
      std::cout << "\n";
    }
    else
    {
      error("Could not open input file '", input_file, "'");
      return EXIT_FAILURE;
    }
  }

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

  return EXIT_SUCCESS;
}

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
#include "repl.hpp"

#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/opium.hpp"

#include <boost/program_options.hpp>

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <string>
#include <cstring>
#include <set>
#include <filesystem>
#include <regex>

namespace std {
namespace fs = std::filesystem;
}


static void
load_prolog_scriptlet(const std::fs::path &path, opi::lisp_parser &parser,
                      opi::prolog_repl &pl)
{
  using namespace opi;

  if (std::ifstream infile {path, std::ios::binary})
  {
    const auto tokens = parser.tokenize(infile, path);
    size_t cursor = 0;
    while (cursor < tokens.size())
    {
      const value expr = parser.parse_tokens(tokens, cursor);
      // Process the expression
      pl << expr;

      // Extract symbols for autocompletion
      extract_symbols(expr);
    }
  }
  else
  {
    error("Could not open input file '{}'", path.c_str());
    throw std::runtime_error {"Can't read file"};
  }
}


static void
read_eval_print_loop(opi::lisp_parser &parser, opi::prolog_repl &pl)
{
  using namespace opi;

  // Initialize readline
  init_readline();

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
      try
      {
        // Process the expression
        pl << expr;
        // Extract symbols for  autocompletion
        extract_symbols(expr);
      }
      catch (const opi::prolog_repl::error &exn)
      {
        std::cout << exn.what() << std::endl;
      }
    }
  }

  // Clean up readline before exiting
  cleanup_readline();
}


/**
 * Strip ANSI escape sequences from a string
 * 
 * \param input The input string containing escape sequences
 * \return The string with escape sequences removed
 */
std::string
strip_escape_sequences(const std::string &input)
{
  // Regex to match escape sequences starting with '\e' and ending with 'm'
  static const std::regex escape_seq_regex("\\\e\\[[^m]*m");
  return std::regex_replace(input, escape_seq_regex, "");
}


int
main(int argc, char **argv)
{
  namespace po = boost::program_options;
  using namespace opi;

  std::string verbosity {loglevel_name(loglevel::info)};
  std::vector<std::string> flags;
  std::vector<std::fs::path> load;
  std::string opath;

  // Define command line options
  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::fs::path>(), "input file to process")
    ("verbosity,v", po::value<std::string>(&verbosity)->implicit_value("debug"), "verbosity")
    ("flag,f", po::value<std::vector<std::string>>(&flags), "flags")
    ("load,l", po::value<std::vector<std::fs::path>>(&load), "load prolog file")
    ("output,o", po::value<std::string>(&opath), "write Scheme script to the specified file")
    ("annotate", "print source code with type-annotations");

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
    error("{}", e.what());
    std::cerr << desc << std::endl;
    return EXIT_FAILURE;
  }

  // Print help
  if (varmap.contains("help"))
  {
    std::cout << "Usage: " << argv[0] << " [options] [input-file]" << std::endl;
    std::cout << desc << std::endl;
    return EXIT_SUCCESS;
  }

  // Set global log-level
  loglevel = parse_loglevel(verbosity);

  // Set global flags
  for (const std::string &flag : flags)
    global_flags.emplace(flag);


  prolog_repl pl;
  lisp_parser parser;

  // Load prolog files
  for (const std::fs::path &path : load)
  {
    info("\e[1mloading Prolog scriptlet from {}\e[0m", path.c_str());
    load_prolog_scriptlet(path, parser, pl);
  }

  // If present, run Type Check on the input file; otherwize, run Prolog REPL
  if (varmap.contains("input-file"))
  {
    const std::fs::path inputpath = varmap["input-file"].as<std::fs::path>();

    std::ifstream inputfile {inputpath, std::ios::binary};
    assert(inputfile.is_open());
    value in = parser.parse_all(inputfile, inputpath);

    scheme_preprocessor pp;
    generate_scheme(in, pp, pl, opath);
  }
  else
    read_eval_print_loop(parser, pl);
  
  execution_timer::report_global_stats();

  return EXIT_SUCCESS;
}

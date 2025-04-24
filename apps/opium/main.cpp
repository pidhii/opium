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
#include "opium/stl/unordered_map.hpp"
#include "repl.hpp"

#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"
#include "opium/pretty_print.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/utilities/execution_timer.hpp"

#include <asm-generic/errno.h>
#include <boost/program_options.hpp>

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <unistd.h>
#include <vector>
#include <string>
#include <cstring>
#include <set>
#include <filesystem>
#include <utility>
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


static void
write_scheme_script(std::ostream &os, opi::value script)
{
  os << "(import (srfi :11))\n\n";
  for (const opi::value expr : range(script))
    os << strip_escape_sequences(pprint_scm(expr)) << "\n\n";
}


using pragmas =
    opi::stl::unordered_map<std::string, opi::stl::deque<opi::value>>;

opi::value
filter_pragmas(opi::value script, pragmas &pragmas)
{
  opi::stl::vector<opi::value> result;
  opi::stl::unordered_map<opi::value, opi::value> matches;
  for (const opi::value expr : range(script))
  {
    // Handle pragmas
    if (expr->t == opi::tag::pair and car(expr) == "pragma")
    {
      if (length(cdr(expr)) < 2 or not issym(car(cdr(expr))))
        throw opi::bad_code {"Invalid pragma", expr};

      const std::string tag {sym_name(car(cdr(expr)))};
      std::ranges::copy(range(cdr(cdr(expr))), std::back_inserter(pragmas[tag]));
    }
    else
    {
      // Pass all other expressions
      result.push_back(expr);
    }
  }

  return list(result);
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
  // - DumpTypeCheck
  // - DebugQuery
  // - DebugQueryVars
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

    // Collect and erase pragmas
    opi::execution_timer pragma_timer {"extraction of pragmas"};
    pragmas pragmas;
    in = filter_pragmas(in, pragmas);
    pragma_timer.stop();

    // Run extra prolog expressions
    for (const value plexpr : pragmas["prolog"])
      pl << plexpr;

    try
    {
      opi::execution_timer preprocessor_timer {"Preprocessor"};
      scheme_preprocessor pp;
      const value ppcode = pp.transform_block(in);
      preprocessor_timer.stop();

      info("\e[1mrunning Type Check on {}\e[0m", inputpath.c_str());
      opi::execution_timer analyzer_timer {"Type analyzer"};
      size_t cnt = 0;
      const auto [out, type_map] =
          translate_to_scheme(cnt, pl, ppcode, pragmas["scheme-translator"]);
      analyzer_timer.stop();

      // Display the source file with type annotations
      if (varmap.contains("annotate"))
      {
        std::ostringstream buf2;
        buf2 << "\e[1mtype-annotated source code:\e[0m\n```";
        std::ifstream infile {inputpath};
        type_map.display_source_with_types(infile, buf2);
        buf2 << "```";
        info("{}", buf2.str());
      }

      // Write generated Scheme script
      if (not opath.empty())
      {
        info("writing Scheme script to {}", opath);
        if (std::ofstream ofile {opath})
          write_scheme_script(ofile, out);
        else
          throw std::runtime_error {
              std::format("Failed to open file {} for writing", opath)};
      }

    }
    catch (const bad_code &exn)
    {
      error("{}", exn.display());
      exit(EXIT_FAILURE);
    }
    catch (const std::runtime_error &exn)
    {
      error("{}", exn.what());
      exit(EXIT_FAILURE);
    }
  }
  else
    read_eval_print_loop(parser, pl);
  
  execution_timer::report_global_stats();

  return EXIT_SUCCESS;
}

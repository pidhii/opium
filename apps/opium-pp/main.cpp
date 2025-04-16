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

#include "opium/lisp_parser.hpp"
#include "opium/source_location.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"
#include "opium/pretty_print.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/utilities/execution_timer.hpp"

#include <boost/program_options.hpp>

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <filesystem>
#include <regex>

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

/**
 * Utility function to print two multi-line strings side-by-side
 * 
 * \param left The left string to print
 * \param right The right string to print
 * \param separator The separator between the two strings (default is "  |  ")
 * \param out The output stream to print to (default is std::cout)
 */
void
print_side_by_side(const std::string &left, const std::string &right,
                   const std::string &separator = "  |  ",
                   size_t min_left_width = 0,
                   std::ostream &out = std::cout)
{
  // Split the strings into lines
  std::vector<std::string> left_lines;
  std::vector<std::string> right_lines;
  
  std::istringstream left_stream(left);
  std::istringstream right_stream(right);
  
  std::string line;
  while (std::getline(left_stream, line)) 
    left_lines.push_back(line);
  
  while (std::getline(right_stream, line)) 
    right_lines.push_back(line);
  
  // Find the maximum width of the left string (without escape sequences)
  size_t max_left_width = min_left_width;
  for (const auto& line : left_lines) 
  {
    std::string stripped_line = strip_escape_sequences(line);
    max_left_width = std::max(max_left_width, stripped_line.length());
  }
  
  // Print the lines side by side
  size_t max_lines = std::max(left_lines.size(), right_lines.size());
  for (size_t i = 0; i < max_lines; ++i)
  {
    // Print left line or empty space if no more lines
    if (i < left_lines.size())
    {
      std::string original_line = left_lines[i];
      std::string stripped_line = strip_escape_sequences(original_line);
      
      out << original_line;
      // Pad to max width (accounting for escape sequences)
      out << std::string(max_left_width - stripped_line.length(), ' ');
    }
    else
    {
      out << std::string(max_left_width, ' ');
    }

    // Print separator
    out << separator;

    // Print right line if available
    if (i < right_lines.size())
      out << right_lines[i];
    out << std::endl;
  }
}

namespace std {
namespace fs = std::filesystem;
}


int
main(int argc, char **argv)
{
  namespace po = boost::program_options;
  using namespace opi;

  std::string verbosity {loglevel_name(loglevel::info)};

  // Define command line options
  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::fs::path>(), "input file to process")
    ("verbosity,v", po::value<std::string>(&verbosity)->implicit_value("debug"), "verbosity level (trace, debug, info, warning, error, critical)");

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
    std::cout << "Usage: " << argv[0] << " [options] input-file" << std::endl;
    std::cout << desc << std::endl;
    return EXIT_SUCCESS;
  }

  // Set global log-level
  loglevel = parse_loglevel(verbosity);

  // Check if input file is provided
  if (not varmap.contains("input-file"))
  {
    error("No input file provided");
    std::cerr << "Usage: " << argv[0] << " [options] input-file" << std::endl;
    std::cerr << desc << std::endl;
    return EXIT_FAILURE;
  }

  const std::fs::path inputpath = varmap["input-file"].as<std::fs::path>();

  // Check if input file exists
  if (not std::fs::exists(inputpath))
  {
    error("Input file '{}' does not exist", inputpath.c_str());
    return EXIT_FAILURE;
  }

  // Parse input file
  lisp_parser parser;
  std::ifstream inputfile {inputpath, std::ios::binary};
  if (not inputfile.is_open())
  {
    error("Could not open input file '{}'", inputpath.c_str());
    return EXIT_FAILURE;
  }

  try
  {
    // Parse the input file
    info("Parsing input file '{}'", inputpath.c_str());
    const value in = parser.parse_all(inputfile, inputpath);

    // Run preprocessor
    info("Running preprocessor");
    opi::execution_timer preprocessor_timer {"Preprocessor"};
    scheme_preprocessor pp;
    const value ppcode = pp.transform_block(in);
    preprocessor_timer.stop();

    // Output the preprocessed code
    info("Preprocessor completed successfully");
    for (const value expr : range(ppcode))
    {
      // Get the expression as a string
      std::string expr_str = pprint_scm(expr);
      
      // Get the location as a string if available
      std::string location_str;
      source_location location;
      if (get_location(expr, location))
        location_str = display_location(location, 1, "", "\e[2m");
      
      // Print expression and location side by side
      print_side_by_side(expr_str, location_str, "  ", 60);
      std::cout << std::endl;
    }
  }
  catch (const code_transformation_error &exn)
  {
    error("{}", exn.display());
    return EXIT_FAILURE;
  }
  catch (const std::runtime_error &exn)
  {
    error("{}", exn.what());
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"
#include "opium/pretty_print.hpp"
#include "opium/scheme/scheme_type_system.hpp"

#include <asm-generic/errno.h>
#include <boost/program_options.hpp>

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <vector>
#include <string>
#include <cstring>
#include <set>
#include <filesystem>

namespace std {
namespace fs = std::filesystem;
}

// Readline headers
#include <readline/readline.h>
#include <readline/history.h>



// Global set to store used symbols for autocompletion
static std::set<std::string> used_symbols;

// Keywords for autocompletion
static const char *keywords[] = {"predicate", "query", "and", "or", nullptr};


// Helper function to read a line with prompt using readline
static bool
prompt_line(const std::string &prompt, std::string &line)
{
  // Use readline to get input with history and editing capabilities
  char *input = readline(prompt.c_str());

  // Check if EOF or error
  if (!input)
    return false;

  // Copy the input to the output string
  line = input;

  // Add non-empty lines to history
  if (not line.empty())
    add_history(input);

  // Free the memory allocated by readline
  free(input);

  return true;
}


// Readline completion function
static char *
command_generator(const char *text, int state)
{
  static size_t keyword_index;
  static std::vector<std::string> matching_symbols;
  static size_t symbol_index;

  // If this is a new word to complete, initialize the counters
  if (state == 0)
  {
    keyword_index = 0;
    matching_symbols.clear();
    symbol_index = 0;

    // Find matching predicates
    const std::string prefix {text};
    for (const auto &pred : used_symbols)
    {
      if (pred.compare(0, prefix.length(), prefix) == 0)
        matching_symbols.push_back(pred);
    }
  }

  // First return matching keywords
  while (keywords[keyword_index])
  {
    const char *name = keywords[keyword_index++];
    if (strncmp(name, text, strlen(text)) == 0)
      return strdup(name);
  }

  // Then return matching predicates
  if (symbol_index < matching_symbols.size())
    return strdup(matching_symbols[symbol_index++].c_str());

  // No more matches
  return nullptr;
}


// Readline completion function
static char **
opium_completion(const char *text, [[maybe_unused]] int start,
                 [[maybe_unused]] int end)
{
  // Don't do filename completion even if our generator finds no matches
  rl_attempted_completion_over = 1;

  // Use our custom word generator function
  return rl_completion_matches(text, command_generator);
}


// Initialize readline with custom settings
static void
init_readline()
{
  // Set application name for history file
  rl_readline_name = "opium";
  
  // Set up custom completion
  rl_attempted_completion_function = opium_completion;
  
  // Enable completion on Tab key
  rl_bind_key('\t', rl_complete);

  // Read history from file if it exists
  read_history(".opium_history");
}


// Save history and clean up readline resources
static void
cleanup_readline()
{
  // Save history to file (limit to 500 entries)
  write_history(".opium_history");
  history_truncate_file(".opium_history", 500);
}


// Extract all non-predicate symbols for auto-completion.
static void
extract_symbols(const opi::value expr)
{
  switch (expr->t)
  {
    case opi::tag::pair:
      extract_symbols(car(expr));
      extract_symbols(cdr(expr));
      break;

    case opi::tag::sym:
      used_symbols.emplace(expr->sym.data);
      break;

    default:
      // Ignore all other objects
      ;
  }
}


int
main(int argc, char **argv)
{
  namespace po = boost::program_options;
  using namespace opi;

  std::string verbosity {loglevel_name(loglevel::warning)};

  // Define command line options
  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::string>(), "input file to process")
    ("verbosity,v", po::value<std::string>(&verbosity)->implicit_value("debug"), "verbosity");

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


  prolog_repl pl;
  lisp_parser parser;
  
  // Read input file if provided
  if (varmap.contains("input-file"))
  {
    const std::fs::path input_file = varmap["input-file"].as<std::string>();
    if (std::ifstream infile {input_file, std::ios::binary})
    {
      const auto tokens = parser.tokenize(infile, input_file);
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
      error("Could not open input file '{}'", input_file.c_str());
      return EXIT_FAILURE;
    }
  }

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
      { std::cout << exn.what() << std::endl; }
    }
  }

  // FIXME (seems to be a bug in C++/GCC)
  // The code below should work when formatters are constructed within arguments
  // of the pretty_printter, i.e. a temporary object is created and the reference
  // to this temporary is passed to the formatter. It seems that some (deleted)
  // copying / move-assignment is taking place if you do it.
  //
  // ```{cpp}
  // pretty_printer pprint_scm {scheme_formatter {}};
  // pretty_printer pprint_pl {prolog_formatter {}};
  // ```
  // NOTE could be due to copy elision
  const scheme_formatter scmfmt;
  const prolog_formatter plfmt;
  pretty_printer pprint_scm {scmfmt};
  pretty_printer pprint_pl {plfmt};

  std::ifstream testfile {"./test.scm", std::ios::binary};
  assert(testfile.is_open());
  const value in = parser.parse_all(testfile, "./test.scm");

  std::cout << "\e[1mscheme input:\e[0m" << std::endl;
  for (const value expr : range(in))
  {
    pprint_scm(std::cout, expr);
    std::cout << std::endl;
  }
  std::cout << std::endl;


  try {
    info("generating prolog type analyser");
    size_t cnt = 0;
    const auto [result, out] = scheme_type_check(cnt, pl, in);

    // Display final Prolog expression generated by type-checker
    pprint_pl(std::cout, out);
    std::cout << std::endl;
    std::cout << std::endl;

    // Gather and display results for each visible variable
    for (const auto &[var, vals] : result)
    {
      std::ostringstream buf;

      // Collect all possible values of the variable
      for (std::string prefix = ""; const value val : vals)
        buf << prefix << "\e[38;5;4m" << val << "\e[0m", prefix = " | ";

      // Choose display style based on whether we have access to source location
      source_location location;
      if (lisp_parser::get_location(var, location))
      {
        std::cout << display_location(location, 1, "\e[38;5;4;4m");
        std::cout << "T = " << buf.str() << std::endl;
      }
      else
        // Simple print with mangled name (glhf)
        std::cout << std::format("untraced variable {} = {}", var, buf.str()) << std::endl;

      std::cout << std::endl;
    }
  }
  catch (const std::runtime_error &exn)
  {
    error("{}", exn.what());
    exit(EXIT_FAILURE);
  }

  // Clean up readline before exiting
  cleanup_readline();

  return EXIT_SUCCESS;
}

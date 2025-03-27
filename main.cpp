#include "opium/lisp_parser.hpp"
#include "opium/lisp_reader.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"
#include "opium/logging.hpp"
#include "opium/code_transformer.hpp"
#include "opium/pretty_print.hpp"
#include "opium/scheme/scheme_transformations.hpp"

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
    const std::string input_file = varmap["input-file"].as<std::string>();
    if (std::ifstream infile {input_file, std::ios::binary})
    {
      const auto tokens = parser.tokenize(infile);
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
      error("Could not open input file '{}'", input_file);
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


  symbol_generator gensym;
  scheme_unique_identifiers makeuids {gensym};
  scheme_code_flattener flatten {gensym};

  pretty_printer pprint {scheme_formatter {}};
  const value in = parser.parse("(let ()                                 "
                                "  (define (print x) (__builtin_print x))"
                                "  (define baz (__builtin_baz x))        "
                                "  (if (input prompt)                    "
                                "      (print (foo bar))                 "
                                "      (let ((z (foo (bar baz)))         "
                                "            (zz 12345))                 "
                                "        (print z))))                    ");
  const value out = compose(makeuids, flatten)(in);

  std::cout << "[test]" << std::endl;
  std::cout << "in:\n", pprint(std::cout, in), std::cout << std::endl;
  std::cout << "out:\n", pprint(std::cout, out), std::cout << std::endl;

  std::cout << "[test quotes]" << std::endl;
  std::cout << parser.parse("`(a b c ,d e ,f ,g h)") << std::endl;;

  // Clean up readline before exiting
  cleanup_readline();

  return EXIT_SUCCESS;
}

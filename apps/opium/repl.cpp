#include "repl.hpp"

#include <set>
#include <string>
#include <vector>

// Readline headers
#include <readline/readline.h>
#include <readline/history.h>



// Global set to store used symbols for autocompletion
static std::set<std::string> used_symbols;

// Keywords for autocompletion
static const char *keywords[] = {"predicate", "query", "and", "or", nullptr};


// Helper function to read a line with prompt using readline
bool
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
_command_generator(const char *text, int state)
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
_opium_completion(const char *text, [[maybe_unused]] int start,
                 [[maybe_unused]] int end)
{
  // Don't do filename completion even if our generator finds no matches
  rl_attempted_completion_over = 1;

  // Use our custom word generator function
  return rl_completion_matches(text, _command_generator);
}


// Initialize readline with custom settings
void
init_readline()
{
  // Set application name for history file
  rl_readline_name = "opium";
  
  // Set up custom completion
  rl_attempted_completion_function = _opium_completion;
  
  // Enable completion on Tab key
  rl_bind_key('\t', rl_complete);

  // Read history from file if it exists
  read_history(".opium_history");
}


// Save history and clean up readline resources
void
cleanup_readline()
{
  // Save history to file (limit to 500 entries)
  write_history(".opium_history");
  history_truncate_file(".opium_history", 500);
}


// Extract all non-predicate symbols for auto-completion.
void
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


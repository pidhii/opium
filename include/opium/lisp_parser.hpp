#pragma once

#include "opium/value.hpp"
#include <string>
#include <vector>
#include <stdexcept>
#include <istream>

/**
 * \file lisp_parser.hpp
 * Lisp parser implementation
 * 
 * This file defines the parser for Lisp-style expressions.
 * 
 * \ingroup lisp
 */


namespace opi {

/**
 * Structure representing a location in the input stream
 * 
 * \ingroup lisp
 *
 * TODO Move to a different file
 */
struct source_location {
  std::string source; ///< Source name (filepath or "<string>")
  size_t start;       ///< Start offset in the input stream
  size_t end;         ///< End offset in the input stream
};


/**
 * Set the source location for a value
 * 
 * \param val Value to set the location for
 * \param loc Source location
 *
 * TODO Move to a different file
 */
void
set_location(value val, source_location loc);

/**
  * Get the source location for a value
  * 
  * \param val Value to get the location for
  * \param[out] location Source location if available
  * \return True if location for \p val is available
  */
[[nodiscard]] bool
get_location(value val, source_location &location);

[[nodiscard]] inline bool
has_location(value val)
{ source_location _; return get_location(val, _); }

/**
 * Copy the source location to another value
 *
 * \param from Value to copy the location from
 * \param to Value to copy the location to
 * \return True if location was copied or if \p from and \p to is the same object
 *
 * TODO Move to a different file
 */
bool
copy_location(opi::value from, opi::value to);


/**
 * Display a fragment of a file according to location with surrounding context
 * and highlighting of the location region
 * 
 * \param location Source location to display
 * \param context_lines Number of context lines to show before and after the location
 * \return Formatted string with the file fragment and highlighting
 *
 * TODO Move to a different file
 */
[[nodiscard]] std::string
display_location(const source_location &location, size_t context_lines = 2,
                 std::string_view hlstyle = "\e[38;5;1;1m",
                 std::string_view ctxstyle = "");

/**
 * Parser for Lisp-style expressions
 * 
 * \ingroup lisp
 */
class lisp_parser {
  public:
  // Parse a LISP-style string into an opi::value
  value
  parse(const std::string &input, const std::string &source_name = "<string>");

  // Parse a LISP-style input stream into an opi::value
  value
  parse(std::istream &input, const std::string &source_name = "<stream>");

  value
  parse_all(std::istream &input, const std::string &source_name = "<stream>");

  /**
   * Token representation for the lexical analyzer
   * 
   * \ingroup lisp
   */
  struct token {
    enum class type {
      LPAREN,         // (
      RPAREN,         // )
      DOT,            // .
      QUOTE,          // '
      QUASIQUOTE,     // `
      UNQUOTE,        // ,
      UNQUOTE_SPLICE, // ,@
      SYMBOL,         // foo, bar, etc.
      STRING,         // "hello"
      NUMBER,         // 123, 3.14
      BOOLEAN,        // #t, #f
      NIL             // nil
    };
    type type;
    std::string value;
    source_location location;
  };

  // Tokenize the input string
  std::vector<token>
  tokenize(const std::string &input, const std::string &source_name = "<string>");

  // Tokenize the input stream
  std::vector<token>
  tokenize(std::istream &input, const std::string &source_name = "<stream>");

  // Parse tokens into a value
  value
  parse_tokens(const std::vector<token> &tokens, size_t &pos);

  // Parse a list
  value
  parse_list(const std::vector<token> &tokens, size_t &pos);

  // Parse an atom (symbol, number, string, etc.)
  value
  parse_atom(const token &token);
  
  // Helper functions
  bool
  is_number(const std::string &s);
  bool
  is_boolean(const std::string &s);
  bool
  is_nil(const std::string &s);

  private:
  value
  _parse_tokens(const std::vector<token> &tokens, size_t &pos);

  value
  _parse_list(const std::vector<token> &tokens, size_t &pos);
};


/**
 * Exception class for parser errors
 * 
 * \ingroup lisp
 */
struct parse_error: public std::runtime_error {
  explicit parse_error(const std::string& what) : std::runtime_error(what) {}
};

} // namespace opi

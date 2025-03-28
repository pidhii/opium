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
 * Parser for Lisp-style expressions
 * 
 * \ingroup lisp
 */
class lisp_parser {
  public:
  // Parse a LISP-style string into an opi::value
  value
  parse(const std::string &input);

  // Parse a LISP-style input stream into an opi::value
  value
  parse(std::istream &input);

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
  };

  // Tokenize the input string
  std::vector<token>
  tokenize(const std::string &input);

  // Tokenize the input stream
  std::vector<token>
  tokenize(std::istream &input);

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

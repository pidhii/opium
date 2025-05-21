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


#pragma once

#include "opium/value.hpp"
#include "opium/source_location.hpp"
#include "opium/exceptions.hpp"

#include <string>
#include <vector>
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
  parse(const std::string &input, const std::string &source_name = "<string>");

  // Parse a LISP-style input stream into an opi::value
  value
  parse(std::istream &input, const std::string &source_name = "<stream>");

  value
  parse_all(const std::string &input, const std::string &source_name = "<string>");

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
struct parse_error: public bad_code {
  using bad_code::bad_code;
};

} // namespace opi

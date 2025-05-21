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
#include "opium/format.hpp" // IWYU pragma: export
#include "opium/stl/deque.hpp"

#include <cctype>
#include <iostream>
#include <sstream>
#include <string>


opi::value
opi::lisp_parser::parse(const std::string &input, const std::string &source_name)
{
  std::vector<token> tokens = tokenize(input, source_name);
  size_t pos = 0;
  if (tokens.empty())
    return nil;
  return parse_tokens(tokens, pos);
}


opi::value
opi::lisp_parser::parse(std::istream &input, const std::string &source_name)
{
  std::vector<token> tokens = tokenize(input, source_name);
  size_t pos = 0;
  if (tokens.empty())
    return nil;
  return parse_tokens(tokens, pos);
}


opi::value
opi::lisp_parser::parse_all(const std::string &input, const std::string &source_name)
{
  const std::vector<token> tokens = tokenize(input, source_name);
  size_t pos = 0;

  stl::deque<value> result;
  while (pos < tokens.size())
    result.push_back(parse_tokens(tokens, pos));

  return list(result);
}


opi::value
opi::lisp_parser::parse_all(std::istream &input, const std::string &source_name)
{
  const std::vector<token> tokens = tokenize(input, source_name);
  size_t pos = 0;

  stl::deque<value> result;
  while (pos < tokens.size())
    result.push_back(parse_tokens(tokens, pos));

  return list(result);
}


std::vector<opi::lisp_parser::token>
opi::lisp_parser::tokenize(const std::string &input, const std::string &source_name)
{
  std::istringstream iss(input);
  return tokenize(iss, source_name);
}


std::vector<opi::lisp_parser::token>
opi::lisp_parser::tokenize(std::istream &input, const std::string &source_name)
{
  std::vector<token> tokens;
  size_t current_pos = 0;
  
  // Read character by character from the stream
  char c;
  while (input.get(c)) {
    // Skip whitespace
    if (std::isspace(c)) {
      current_pos++;
      continue;
    }

    // Handle comments (semicolon to end of line)
    if (c == ';') {
      current_pos++;
      while (input.get(c) && c != '\n') {
        current_pos++;
      }
      if (c == '\n') current_pos++;
      continue;
    }

    // Handle parentheses
    if (c == '(') {
      size_t start = current_pos;
      current_pos++;
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({token::type::LPAREN, "(", loc});
      continue;
    }
    if (c == ')') {
      size_t start = current_pos;
      current_pos++;
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({token::type::RPAREN, ")", loc});
      continue;
    }

    // Handle dot
    if (c == '.') {
      // Peek at the next character to see if it's whitespace or a closing paren
      char next = input.peek();
      if (std::isspace(next) or next == ')') {
        size_t start = current_pos;
        current_pos++;
        source_location loc = {source_name, start, current_pos};
        tokens.push_back({token::type::DOT, ".", loc});
        continue;
      }
      // Otherwise, it's part of a symbol or number, so put it back
      input.putback(c);
      c = input.peek();
    }

    // Handle quote
    if (c == '\'') {
      size_t start = current_pos;
      current_pos++;
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({token::type::QUOTE, "'", loc});
      continue;
    }

    // Handle quasiquote (backtick)
    if (c == '`') {
      size_t start = current_pos;
      current_pos++;
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({token::type::QUASIQUOTE, "`", loc});
      continue;
    }

    // Handle unquote and unquote-splicing
    if (c == ',') {
      size_t start = current_pos;
      current_pos++;
      // Check if it's unquote-splicing (,@)
      char next = input.peek();
      if (next == '@') {
        input.get(); // Consume the '@'
        current_pos++;
        source_location loc = {source_name, start, current_pos};
        tokens.push_back({token::type::UNQUOTE_SPLICE, ",@", loc});
      } else {
        source_location loc = {source_name, start, current_pos};
        tokens.push_back({token::type::UNQUOTE, ",", loc});
      }
      continue;
    }

    // Handle strings
    if (c == '"') {
      size_t start = current_pos;
      current_pos++;
      std::string str;
      bool escaped = false;
      
      while (input.get(c) && (c != '"' || escaped)) {
        current_pos++;
        if (escaped) {
          switch (c) {
            case 'n': str += '\n'; break;
            case 't': str += '\t'; break;
            case 'r': str += '\r'; break;
            case '"': str += '"'; break;
            case '\\': str += '\\'; break;
            default: str += c; break;
          }
          escaped = false;
        } else if (c == '\\') {
          escaped = true;
        } else {
          str += c;
        }
      }
      
      if (c != '"') {
        throw parse_error("Unterminated string literal");
      }
      current_pos++;
      
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({token::type::STRING, str, loc});
      continue;
    }

    // Handle symbols, numbers, booleans, and nil
    size_t start = current_pos;
    std::string atom;
    atom += c;
    current_pos++;

    while (input.get(c) and
           not std::isspace(c) and
           c != '(' and c != ')' and 
           c != ';' and c != '"' and c != '\'' and
           c != '`' and c != ',') {
      atom += c;
      current_pos++;
    }
    
    // Put back the last character that didn't match
    if (not input.eof()) {
      input.putback(c);
    }

    if (!atom.empty())
    {
      enum token::type type;
      if (is_number(atom))
        type = token::type::NUMBER;
      else if (is_boolean(atom))
        type = token::type::BOOLEAN;
      else if (is_nil(atom))
        type = token::type::NIL;
      else
        type = token::type::SYMBOL;
      
      source_location loc = {source_name, start, current_pos};
      tokens.push_back({type, atom, loc});
    }
  }

  return tokens;
}


opi::value
opi::lisp_parser::_parse_tokens(const std::vector<token> &tokens, size_t &pos)
{
  if (pos >= tokens.size())
    throw parse_error {"Unexpected end of input"};

  const token &tok = tokens[pos];
  const source_location &loc = tok.location;
  pos++;

  value result = nil;
  
  switch (tok.type)
  {
    case token::type::LPAREN: {
      result = parse_list(tokens, pos);
      break;
    }

    case token::type::QUOTE: {
      if (pos >= tokens.size())
        throw parse_error {"Unexpected end of input after quote"};
      value quoted = parse_tokens(tokens, pos);
      result = list(sym("quote"), quoted);
      break;
    }

    case token::type::QUASIQUOTE: {
      if (pos >= tokens.size())
        throw parse_error {"Unexpected end of input after quasiquote"};
      value quoted = parse_tokens(tokens, pos);
      result = list(sym("quasiquote"), quoted);
      break;
    }

    case token::type::UNQUOTE: {
      if (pos >= tokens.size())
        throw parse_error {"Unexpected end of input after unquote"};
      value unquoted = parse_tokens(tokens, pos);
      result = list(sym("unquote"), unquoted);
      break;
    }

    case token::type::UNQUOTE_SPLICE: {
      if (pos >= tokens.size())
        throw parse_error {"Unexpected end of input after unquote-splicing"};
      value spliced = parse_tokens(tokens, pos);
      result = list(sym("unquote-splicing"), spliced);
      break;
    }

    default:
      result = parse_atom(tok);
      // For atoms, we already have the location from the token
      set_location(result, tok.location);
      return result;
  }
  
  // For non-atoms, we need to find the end position
  // If we've reached the end of tokens, use the last token's end position
  source_location end_loc = (pos < tokens.size()) ? tokens[pos-1].location : tokens.back().location;
  
  // Set the location for the result
  source_location result_loc = {
    loc.source,
    loc.start,
    end_loc.end
  };
  set_location(result, result_loc);
  
  return result;
}


opi::value
opi::lisp_parser::_parse_list(const std::vector<token> &tokens, size_t &pos)
{
  if (pos >= tokens.size())
    throw parse_error {"Unexpected end of input while parsing list"};

  // Handle empty list
  if (tokens[pos].type == token::type::RPAREN)
  {
    const source_location &loc = tokens[pos].location;
    pos++;
    value result = nil;
    set_location(result, loc);
    return result;
  }

  // Parse first element
  value car_val = parse_tokens(tokens, pos);

  if (pos >= tokens.size())
    throw parse_error {"Unexpected end of input while parsing list"};

  // Check for dot notation (improper list)
  if (tokens[pos].type == token::type::DOT)
  {
    pos++; // Skip dot
    if (pos >= tokens.size())
      throw parse_error {"Unexpected end of input after dot"};

    // Parse the cdr
    value cdr_val = parse_tokens(tokens, pos);

    // Expect closing parenthesis
    if (pos >= tokens.size() or tokens[pos].type != token::type::RPAREN)
      throw parse_error {"Expected closing parenthesis after dotted pair"};
    
    // Get the location from the closing parenthesis
    const source_location &rparen_loc = tokens[pos].location;
    pos++; // Skip closing paren

    // Get the location from car_val
    value result = pair(car_val, cdr_val);
    source_location car_loc;
    if (get_location(car_val, car_loc))
    {
      source_location result_loc = {
        car_loc.source,
        car_loc.start,
        rparen_loc.end
      };
      set_location(result, result_loc);
    }
    return result;
  }
  else
  {
    // Regular list
    value cdr_val = parse_list(tokens, pos);
    value result = pair(car_val, cdr_val);

    // Get locations for car and cdr
    source_location car_loc, cdr_loc;
    if (get_location(car_val, car_loc))
    {
      // For empty lists, cdr might not have a location
      source_location result_loc;
      result_loc.source = car_loc.source;
      result_loc.start = car_loc.start;

      if (get_location(cdr_val, cdr_loc)) {
        result_loc.end = cdr_loc.end;
      } else if (pos > 0 && pos <= tokens.size()) {
        result_loc.end = tokens[pos-1].location.end;
      } else {
        result_loc.end = tokens.back().location.end;
      }

      set_location(result, result_loc);
    }
    
    return result;
  }
}


opi::value
opi::lisp_parser::parse_atom(const token &tok)
{
  value result = nil;
  
  switch (tok.type)
  {
    case token::type::SYMBOL:
      result = sym(tok.value);
      break;

    case token::type::STRING:
      result = str(tok.value);
      break;

    case token::type::NUMBER: {
      char *end;
      long double value = std::strtold(tok.value.c_str(), &end);
      if (*end != '\0')
        throw parse_error {std::format("Invalid number: {}", tok.value)};
      result = num(value);
      break;
    }

    case token::type::BOOLEAN:
      result = tok.value == "#t" ? True : False;
      break;

    case token::type::NIL:
      result = nil;
      break;

    default:
      throw parse_error { 
          std::format("Unexpected token type: {}", static_cast<int>(tok.type)) };
  }
  
  // Set the location for the atom
  set_location(result, tok.location);
  
  return result;
}


bool
opi::lisp_parser::is_number(const std::string &s)
{
  if (s.empty())
    return false;

  // Check if the string is a valid number
  char *end;
  std::strtold(s.c_str(), &end);
  return *end == '\0';
}


bool
opi::lisp_parser::is_boolean(const std::string &s)
{ return s == "#t" or s == "#f"; }


bool
opi::lisp_parser::is_nil(const std::string &s)
{ return s == "()"; }


opi::value
opi::lisp_parser::parse_tokens(const std::vector<token> &tokens, size_t &pos)
{
  // Wrap actual tokenizer to preserve `pos` argument upon exception
  size_t proxypos = pos;
  const value result = _parse_tokens(tokens, proxypos);
  pos = proxypos;
  return result;
}


opi::value
opi::lisp_parser::parse_list(const std::vector<token> &tokens, size_t &pos)
{
  // Wrap actual tokenizer to preserve `pos` argument upon exception
  size_t proxypos = pos;
  const value result = _parse_list(tokens, proxypos);
  pos = proxypos;
  return result;
}

#include "opium/lisp_parser.hpp"
#include "opium/format.hpp"

#include <cctype>
#include <iostream>
#include <sstream>
#include <string>


opi::value
opi::lisp_parser::parse(const std::string &input)
{
  std::vector<token> tokens = tokenize(input);
  size_t pos = 0;
  if (tokens.empty())
    return nil;
  return parse_tokens(tokens, pos);
}


opi::value
opi::lisp_parser::parse(std::istream &input)
{
  std::vector<token> tokens = tokenize(input);
  size_t pos = 0;
  if (tokens.empty())
    return nil;
  return parse_tokens(tokens, pos);
}


std::vector<opi::lisp_parser::token>
opi::lisp_parser::tokenize(const std::string &input)
{
  std::istringstream iss(input);
  return tokenize(iss);
}


std::vector<opi::lisp_parser::token>
opi::lisp_parser::tokenize(std::istream &input)
{
  std::vector<token> tokens;
  
  // Read character by character from the stream
  char c;
  while (input.get(c)) {
    // Skip whitespace
    if (std::isspace(c)) {
      continue;
    }

    // Handle comments (semicolon to end of line)
    if (c == ';') {
      while (input.get(c) && c != '\n') {
        // Skip until end of line
      }
      continue;
    }

    // Handle parentheses
    if (c == '(') {
      tokens.push_back({token::type::LPAREN, "("});
      continue;
    }
    if (c == ')') {
      tokens.push_back({token::type::RPAREN, ")"});
      continue;
    }

    // Handle dot
    if (c == '.') {
      // Peek at the next character to see if it's whitespace or a closing paren
      char next = input.peek();
      if (std::isspace(next) or next == ')') {
        tokens.push_back({token::type::DOT, "."});
        continue;
      }
      // Otherwise, it's part of a symbol or number, so put it back
      input.putback(c);
      c = input.peek();
    }

    // Handle quote
    if (c == '\'') {
      tokens.push_back({token::type::QUOTE, "'"});
      continue;
    }

    // Handle strings
    if (c == '"') {
      std::string str;
      bool escaped = false;
      
      while (input.get(c) && (c != '"' || escaped)) {
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
      
      tokens.push_back({token::type::STRING, str});
      continue;
    }

    // Handle symbols, numbers, booleans, and nil
    std::string atom;
    atom += c;

    while (input.get(c) and
           not std::isspace(c) and
           c != '(' and c != ')' and 
           c != ';' and c != '"' and c != '\'')
      atom += c;
    
    // Put back the last character that didn't match
    if (not input.eof())
      input.putback(c);

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
      tokens.push_back({type, atom});
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
  pos++;

  switch (tok.type)
  {
    case token::type::LPAREN:
      return parse_list(tokens, pos);

    case token::type::QUOTE: {
      if (pos >= tokens.size())
        throw parse_error {"Unexpected end of input after quote"};
      value quoted = parse_tokens(tokens, pos);
      return list(sym("quote"), quoted);
    }

    default:
      return parse_atom(tok);
  }
}


opi::value
opi::lisp_parser::_parse_list(const std::vector<token> &tokens, size_t &pos)
{
  if (pos >= tokens.size())
    throw parse_error {"Unexpected end of input while parsing list"};

  // Handle empty list
  if (tokens[pos].type == token::type::RPAREN)
  {
    pos++;
    return nil;
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
    pos++; // Skip closing paren

    return pair(car_val, cdr_val);
  }
  else
  {
    // Regular list
    value cdr_val = parse_list(tokens, pos);
    return pair(car_val, cdr_val);
  }
}


opi::value
opi::lisp_parser::parse_atom(const token &tok)
{
  switch (tok.type)
  {
    case token::type::SYMBOL:
      return sym(tok.value);

    case token::type::STRING:
      return str(tok.value);

    case token::type::NUMBER: {
      char *end;
      long double value = std::strtold(tok.value.c_str(), &end);
      if (*end != '\0')
        throw parse_error {std::format("Invalid number: {}", tok.value)};
      return num(value);
    }

    case token::type::BOOLEAN:
      return tok.value == "#t" ? True : False;

    case token::type::NIL:
      return nil;

    default:
      throw parse_error { 
          std::format("Unexpected token type: {}", static_cast<int>(tok.type)) };
  }
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
{ return s == "nil" or s == "()"; }


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

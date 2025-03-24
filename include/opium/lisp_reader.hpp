#pragma once

#include "opium/lisp_parser.hpp"
#include "opium/stl/deque.hpp"
#include "opium/value.hpp"

#include <string>
#include <vector>

/**
 * \file lisp_reader.hpp
 * Lisp reader implementation
 * 
 * This file defines a reader for Lisp-style expressions that allows
 * gradual parsing of text fragments.
 * 
 * \ingroup lisp
 */


namespace opi {

/**
 * Utility class allowing gradual parsing of text fragments into LISP
 * 
 * \ingroup lisp
 */
class lisp_reader {
  public:
  lisp_reader(lisp_parser &parser);

  void
  operator << (const std::string &input);

  bool
  operator >> (value &result);

  private:
  lisp_parser &m_parser;
  std::vector<lisp_parser::token> m_tokens;
  opi::deque<value> m_values;
}; // class opi::lisp_reader

} // namespace opi

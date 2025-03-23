#pragma once

#include "opium/match.hpp"
#include "opium/value.hpp"
#include "opium/stl/deque.hpp"

#include <functional>
#include <stdexcept>


namespace opi {


struct code_transformation_error: public std::runtime_error {
  using std::runtime_error::runtime_error;
}; // struct opi::code_transformation_error


class code_transformer {
  public:
  using match_mapping = opi::unordered_map<value, value>;
  using transformation = std::function<value(const match_mapping&)>;

  void
  prepend_rule(const match &matcher, const transformation &transformer);

  void
  append_rule(const match &matcher, const transformation &transformer);

  value
  operator () (value inexpr) const;

  private:
  opi::deque<std::pair<match, transformation>> m_syntax_table; /**< Syntax table */
}; // class opi::code_transformer


struct scheme_code_transformer: public code_transformer {
  scheme_code_transformer();
}; // struct opi::scheme_code_transformer


} // namespace opi

#pragma once

#include "opium/code_transformer.hpp"
#include "opium/code_transform_utils.hpp"


namespace opi {

class scheme_unique_identifiers: public scheme_code_transformer {
  public:
  scheme_unique_identifiers(symbol_generator &gensym);

  private:
  symbol_generator &m_gensym;
  value m_alist;
}; // class opi::scheme_unique_identifiers


class scheme_code_flattener: public scheme_code_transformer {
  public:
  scheme_code_flattener(symbol_generator &gensym);

  private:
  symbol_generator &m_gensym;
}; // clss opi::scheme_code_flattener


class scheme_to_prolog: public code_transformer {
  public:
  using type_format_string = std::format_string<std::string_view>;

  scheme_to_prolog(type_format_string format = "T:{}");

  private:
  type_format_string m_type_format;
}; // class opi::scheme_to_prolog

} // namespace opi
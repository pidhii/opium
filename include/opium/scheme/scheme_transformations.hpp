#pragma once

#include "opium/code_transformer.hpp"
#include "opium/code_transform_utils.hpp"
#include "opium/value.hpp"


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
  using type_format_string = std::format_string<std::string>;

  scheme_to_prolog(type_format_string format = "T:{}");

  value
  transform_block(value block);

  protected:
  value
  _to_type(value ident) const;

  private:
  type_format_string m_type_format;
  value m_target;
  value m_alist;
}; // class opi::scheme_to_prolog

} // namespace opi
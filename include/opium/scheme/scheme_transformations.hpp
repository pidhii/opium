#pragma once

#include "opium/code_transformer.hpp"
#include "opium/code_transform_utils.hpp"


namespace opi {

class scheme_unique_identifiers: public code_transformer {
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

} // namespace opi
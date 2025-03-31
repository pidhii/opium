#pragma once

#include "opium/code_transformer.hpp"
#include "opium/code_transform_utils.hpp"
#include "opium/value.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/list.hpp"


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
  struct unknown_identifier: public std::runtime_error {
    using std::runtime_error::runtime_error;
  }; // struct opi::scheme_to_prolog::unknown_identifier

  using type_format_string = std::format_string<std::string>;

  scheme_to_prolog(type_format_string format = "T:{}");

  void
  add_global(value ident, value type) noexcept
  { m_global_alist = cons(cons(ident, type), m_global_alist); }

  value
  transform_block(value block);

  std::ranges::range auto
  predicates() const
  { return m_predicates; }

  protected:
  value
  _lambda(std::string_view name, value parmlist, value body);

  std::string
  _type_name_for(value ident) const;

  value
  _require_symbol(value ident);

  value
  _to_type(value atom);

  private:
  type_format_string m_type_format;
  value m_target;
  value m_alist;
  value m_global_alist;
  value m_unresolved;
  stl::list<predicate> m_predicates;
  symbol_generator m_lambda_gensym;
}; // class opi::scheme_to_prolog

} // namespace opi
#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/stl/list.hpp"
#include "opium/value.hpp"

namespace opi {

class scheme_unique_identifiers: public scheme_code_transformer {
  public:
  scheme_unique_identifiers(symbol_generator &gensym, bool is_toplevel = true);

  private:
  // Helper for automatic reset of toplevel-flag in transformation rules
  value
  _T(value expr);
  std::function<value(value)> T;

  private:
  symbol_generator &m_gensym;
  value m_alist;
  bool m_is_toplevel;
}; // class opi::scheme_unique_identifiers


class scheme_code_flattener: public scheme_code_transformer {
  public:
  scheme_code_flattener(symbol_generator &gensym);

  private:
  symbol_generator &m_gensym;
}; // class opi::scheme_code_flattener


class scheme_to_prolog: public code_transformer {
  public:
  struct unknown_identifier: public code_transformation_error {
    using code_transformation_error::code_transformation_error;
  }; // struct opi::scheme_to_prolog::unknown_identifier

  struct duplicate_code_objects: public code_transformation_error {
    using code_transformation_error::code_transformation_error;
  }; // struct opi::scheme_to_prolog::duplicate_code_objects

  using type_format_string = std::format_string<std::string>;

  scheme_to_prolog(size_t &counter, type_format_string format = "T:{}");

  void
  add_global(value ident, value type) noexcept
  { m_global_alist = cons(cons(ident, type), m_global_alist); }

  value
  transform_block(value block);

  std::ranges::view auto
  predicates() const
  { return std::views::all(m_predicates); }

  std::ranges::range auto
  unresolved() const
  { return range(m_unresolved) | std::views::transform(car<false>); }

  bool
  find_code_type(value code, value &type) const noexcept;

  value
  find_code_type(value code) const;

  bool
  find_overload_name(value code, value &name) const noexcept;

  value
  find_overload_name(value code) const;

  bool
  is_overload_name(value name) const noexcept;

  protected:
  /**
   * Link a code-object (usually identifier) with generated type identifier
   */
  void
  _link_code_to_type(value code, value type);

  void
  _link_code_to_overload_name(value code_identifier, value name);

  value
  _create_template(std::string_view name, value parmlist, value body);

  std::string
  _type_name_for(value ident) const;

  value
  _generate_type_and_copy_location(value ident);

  template <std::output_iterator<value> CodeOutput>
  value
  _require_symbol(value ident, CodeOutput code_output);

  template <std::output_iterator<value> CodeOutput>
  value
  _to_type(value atom, bool resolve_symbols, CodeOutput code_output);

  template <std::output_iterator<value> CodeOutput>
  value
  _instantiate_template(value ident, value closure, size_t arity,
                        CodeOutput out);

  template <std::output_iterator<value> CodeOutput>
  value
  _instantiate_template(value type_template, CodeOutput out);

  private:
  type_format_string m_type_format;
  value m_target;
  value m_alist;
  value m_global_alist;
  value m_unresolved;
  stl::deque<predicate> m_predicates;
  symbol_generator m_lambda_gensym;
  stl::unordered_map<void *, value> m_type_map; /**< Associations between input
                                                     symbols and generated
                                                     typenames */
  stl::unordered_map<void *, value> m_overloads;
}; // class opi::scheme_to_prolog

} // namespace opi

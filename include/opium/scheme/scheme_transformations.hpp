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

#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/value.hpp"


namespace opi {


// TODO: this guy transforms `define` and `define-overload` into `template`;
//       it has to be done by a separate transformer
// TODO: remane to `scheme_renamer`
class scheme_unique_identifiers: public ext_scheme_code_transformer {
  public:
  scheme_unique_identifiers(symbol_generator &gensym);

  value
  transform_block(value block) const;

  private:
  value
  _copy_mapped_identifier(value identifier) const;

  value
  _into_unique_symbol(value identifier, std::string_view prefix = "") const;

  // Helper for automatic reset of toplevel-flag in transformation rules
  value
  _T(value expr);
  std::function<value(value)> T;

  private:
  symbol_generator &m_gensym;
  mutable value m_alist;
  mutable value m_overload_alist;
}; // class opi::scheme_unique_identifiers


class scheme_code_flattener: public ext_scheme_code_transformer {
  public:
  scheme_code_flattener(symbol_generator &gensym);

  private:
  symbol_generator &m_gensym;
}; // class opi::scheme_code_flattener


class scheme_preprocessor {
  public:
  scheme_preprocessor(symbol_generator &gensym)
  : m_flattener {gensym},
    m_unique_identifiers {gensym}
  { }

  scheme_preprocessor()
  : m_default_counter {0},
    m_default_symbol_generator {{m_default_counter.value(), "uid{}"}},
    m_flattener {m_default_symbol_generator.value()},
    m_unique_identifiers {m_default_symbol_generator.value()}
  { }

  value
  operator () (value expr) const
  { return m_unique_identifiers(m_flattener(expr)); }

  value
  transform_block(value block) const
  {
    return m_unique_identifiers.transform_block(
        opi::transform_block(m_flattener, block));
  }

  private:
  std::optional<size_t> m_default_counter;
  std::optional<symbol_generator> m_default_symbol_generator;
  scheme_code_flattener m_flattener;
  scheme_unique_identifiers m_unique_identifiers;
}; // class opi::scheme_preprocessor
static_assert(transformation<scheme_preprocessor>);


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
  set_up_prolog(prolog &pl) const noexcept;

  void
  add_global(value ident, value type) noexcept
  { m_global_alist = cons(cons(ident, type), m_global_alist); }

  value
  transform_block(value block);

  bool
  find_code_type(value code, value &type) const noexcept;

  value
  find_code_type(value code) const;

  protected:
  /**
   * Link a code-object (usually identifier) with generated type identifier
   */
  void
  _link_code_to_type(value code, value type);

  value
  _create_template(value param_symbols, value resul_symbolt, value body);

  std::string
  _type_name_for(value ident) const;

  value
  _generate_type_and_copy_location(value ident);

  template <std::output_iterator<value> CodeOutput>
  value
  _require_symbol(value ident, CodeOutput code_output, bool lvalue = false);

  template <std::output_iterator<value> CodeOutput>
  value
  _to_type(value atom, bool resolve_symbols, CodeOutput code_output);

  private:
  type_format_string m_type_format;
  bool m_is_template;
  value m_target;
  value m_alist;
  value m_global_alist;
  symbol_generator m_lambda_gensym;
  stl::unordered_map<void *, value> m_type_map; /**< Associations between input
                                                     symbols and generated
                                                     typenames */
}; // class opi::scheme_to_prolog

} // namespace opi

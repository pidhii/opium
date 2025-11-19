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
class scheme_unique_identifiers: public ext_scheme_code_transformer {
  public:
  scheme_unique_identifiers(
      symbol_generator &gensym,
      const std::optional<std::string> &norename_prefix = std::nullopt);

  void
  set_norename_prefix(std::string_view prefix)
  { m_norename_prefix = prefix; }

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
  std::optional<std::string> m_norename_prefix;
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

  void
  set_norename_prefix(std::string_view prefix)
  { m_unique_identifiers.set_norename_prefix(prefix); }

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


struct code_type_map {
  void
  assign_type(value code, value type)
  {
    if (not m_data.emplace(&*code, type).second)
    {
      // Ignore clash if replacing with identical type anyways
      const value oldtype = m_data.at(&*code);
      if (type == oldtype)
        return;

      throw bad_code {
          std::format("Can't link code to type (duplicate code object); would "
                      "replace '{}' with '{}'",
                      oldtype, type),
          code};
    }
  }

  bool
  code_type(value code, value &type) const noexcept
  {
    const auto it = m_data.find(&*code);

    // Check if code object is present in the map
    if (it == m_data.end())
      return false;

    // Return associated type
    type = it->second;
    return true;
  }

  value
  code_type(value code) const
  {
    value result = nil;
    if (code_type(code, result))
      return result;
    throw bad_code {
        std::format("No type associated to code object"), code};
  }

  private:
  stl::unordered_map<void *, value> m_data; /**< Associations between input
                                                symbols and generated
                                                typenames */
};

class scheme_to_prolog: public code_transformer {
  public:
  struct unknown_identifier: public code_transformation_error {
    using code_transformation_error::code_transformation_error;
  }; // struct opi::scheme_to_prolog::unknown_identifier

  struct duplicate_code_objects: public code_transformation_error {
    using code_transformation_error::code_transformation_error;
  }; // struct opi::scheme_to_prolog::duplicate_code_objects

  scheme_to_prolog(size_t &counter, code_type_map &code_types);

  static void
  setup_prolog(prolog &pl);

  void
  add_global(value ident, value type);

  value
  transform_block(value block);

  template <std::output_iterator<value> CodeOutput>
  value
  to_type(value atom, bool resolve_symbols, CodeOutput code_output);

  protected:
  value
  _create_template(value param_symbols, value resul_symbolt, value body);

  std::string
  _type_name_for(value ident) const;

  value
  _generate_type_and_copy_location(value ident);

  template <std::output_iterator<value> CodeOutput>
  value
  _require_symbol(value ident, CodeOutput code_output, bool lvalue = false);

  private:
  value m_targets;
  value m_alist;
  value m_global_alist;
  code_type_map &m_ctm;
  symbol_generator m_typevargen;
  symbol_generator m_termgen;
}; // class opi::scheme_to_prolog


struct scheme_syntax_plugin {
  scheme_syntax_plugin();

  virtual
  ~scheme_syntax_plugin();

  virtual void
  load(scheme_unique_identifiers &transformer) = 0;

  virtual void
  load(scheme_code_flattener &transformer) = 0;

  virtual void
  load(scheme_to_prolog &transformer) = 0;

  template <typename Transformer>
  static void
  apply_all(Transformer &transformer)
  {
    for (scheme_syntax_plugin *plug : m_plugins)
      plug->load(transformer);
  }

  static std::ranges::view auto
  all()
  { return std::views::all(m_plugins); }

  private:
  static stl::list<scheme_syntax_plugin*> m_plugins;
};


} // namespace opi

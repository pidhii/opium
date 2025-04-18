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

#include "opium/scheme/scheme_transformations.hpp"
#include "opium/prolog.hpp"
#include "opium/value.hpp"


namespace opi {


using code_tape = opi::stl::vector<value>;


using code_tape_output = std::back_insert_iterator<code_tape>;


struct scheme_emitter_context;

struct function_template {
  value ppdefinition;
  value typetemplate;
  scheme_emitter_context &context;
};


struct case_to_scheme {
  match ctor_match;
  match type_match;
  value predicate;
  value unpack;
};


struct scheme_emitter_context {
  scheme_emitter_context(const prolog &pl, const scheme_to_prolog &pl_emitter,
                         code_tape &output);

  scheme_emitter_context(scheme_emitter_context &parent, code_tape &output);

  bool
  has_parent() const noexcept
  { return &m_parent != this; }

  bool
  has_template(value tag) const noexcept;
  
  const function_template
  find_template(value tag) const;
  
  void
  register_template(value tag, const function_template &functemplate);
  

  /**
   * Reigster specialization of a function template for future reuse
   *
   * \param tag Template tag
   * \param type Specialization type
   * \param identifier Identifier of the specialized function
   */
  void
  register_function_template_specialization(value tag, value type,
                                            value identifier);
  
  bool
  find_function_template_speciailization(value type,
                                         value &identifier) const noexcept;

  bool
  identifier_refers_to_function_template(value identifier) const noexcept;

  void
  register_identifier_for_function_template(value identifier);

  void
  add_case_rule(const case_to_scheme &rule) noexcept;

  const case_to_scheme&
  find_case_rule(value pattern, value type) const;

  /**
   * Get output tape for supplementary code
   */
  code_tape_output &
  output()
  { return m_output; }

  /**
   * Get reference to the prolog instance
   */
  const prolog &
  pl() const
  { return m_pl; }

  /**
   * Get reference to the prolog emitter
   */
  const scheme_to_prolog &
  prolog_emitter() const
  { return m_prolog_emitter; }

private:
  code_tape_output m_output; /**< Output tape for supplementary code */

  /** Identifiers to be treated as references to function templates */
  opi::stl::unordered_set<value> m_function_template_identifiers;

  /** Definitions of function templates */
  opi::stl::unordered_map<value /* code tag */, function_template> m_templates;

  /** Cache for produced template specializations */
  opi::stl::unordered_map<value /* type */, value /* function identifier */>
      m_specializations;

  /** Rules on handling matching in cases expressions */
  opi::stl::deque<case_to_scheme> m_cases_rules;

  const prolog &m_pl; /**< Storage for predicates */
  const scheme_to_prolog &m_prolog_emitter; /**< Storage for type info */

  scheme_emitter_context &m_parent;
}; // struct opi::scheme_emitter_context


} // namespace opi

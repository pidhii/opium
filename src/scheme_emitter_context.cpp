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


#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/scheme/scheme_transformations.hpp"


opi::scheme_emitter_context::scheme_emitter_context(
    const prolog &pl, const code_type_map &ctm, code_tape &output)
: m_output {std::back_inserter(output)},
  m_pl {pl},
  m_code_types {ctm},
  m_parent {*this}
{ }


opi::scheme_emitter_context::scheme_emitter_context(
    scheme_emitter_context &parent, code_tape &output)
: m_output {std::back_inserter(output)},
  m_pl {parent.pl()},
  m_code_types {parent.ctm()},
  m_parent {parent}
{ }


bool
opi::scheme_emitter_context::has_template(value tag) const noexcept
{
  if (m_templates.contains(tag))
    return true;
  return has_parent() and m_parent.has_template(tag);
}


const opi::function_template
opi::scheme_emitter_context::find_template(value tag) const
{
  const auto it = m_templates.find(tag);
  if (it != m_templates.end())
    return it->second;
  if (has_parent())
    return m_parent.find_template(tag);
  throw std::range_error {std::format("No template with tag {}", tag)};
}


void
opi::scheme_emitter_context::register_template(
    value tag, const function_template &functemplate)
{
  if (has_template(tag))
    throw std::invalid_argument {
        std::format("Duplica template definition (tag: {})", tag)};
  m_templates.emplace(tag, functemplate);
}


/**
  * Reigster specialization of a function template for future reuse
  *
  * \param tag Template tag
  * \param type Specialization type
  * \param identifier Identifier of the specialized function
  */
void
opi::scheme_emitter_context::register_function_template_specialization(
    value tag, value type, value identifier)
{
  if (not has_template(tag))
    throw std::invalid_argument {std::format(
        "Register specialization for non-existent template {}", tag)};
  assert(m_templates.contains(tag) and
          "Template and its specialization must belong to the same context");
  m_specializations.emplace_back(type, identifier);
}


bool
opi::scheme_emitter_context::find_function_template_speciailization(
    value type, value &identifier) const noexcept
{
  const auto it =
      std::ranges::find_if(m_specializations, [&type](const auto &spec_ident) {
        return equivalent_up_to_bindings(type, spec_ident.first);
      });
  if (it == m_specializations.end())
    return false;
  identifier = it->second;
  return true;
}


bool
opi::scheme_emitter_context::identifier_refers_to_function_template(
    value identifier) const noexcept
{
  if (m_function_template_identifiers.contains(identifier))
    return true;
  return has_parent() and
          m_parent.identifier_refers_to_function_template(identifier);
}


void
opi::scheme_emitter_context::register_identifier_for_function_template(
    value identifier)
{
  // NOTE: duplicate insertions occure naturally due to function overloads
  m_function_template_identifiers.emplace(identifier);
}


void
opi::scheme_emitter_context::add_case_rule(const case_to_scheme &rule) noexcept
{ m_cases_rules.emplace_back(rule); }


const opi::case_to_scheme&
opi::scheme_emitter_context::find_case_rule(value pattern, value type) const
{
  const auto it = std::ranges::find_if(m_cases_rules, [&](const auto &rule) {
    return rule.ctor_match(pattern) and rule.type_match(type);
  });
  if (it != m_cases_rules.end())
    return *it;

  if (has_parent())
    return m_parent.find_case_rule(pattern, type);

  throw bad_code {
      std::format("No handler for case {} with type {}", pattern, type)};
}

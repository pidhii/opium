#include "opium/scheme/scheme_emitter_context.hpp"


opi::scheme_emitter_context::scheme_emitter_context(
    const prolog &pl, const scheme_to_prolog &pl_emitter, code_tape &output)
: m_output {std::back_inserter(output)},
  m_pl {pl},
  m_prolog_emitter {pl_emitter},
  m_parent {*this}
{ }


opi::scheme_emitter_context::scheme_emitter_context(
    scheme_emitter_context &parent, code_tape &output)
: m_output {std::back_inserter(output)},
  m_pl {parent.pl()},
  m_prolog_emitter {parent.prolog_emitter()},
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
// TODO: move to a cpp file
void
opi::scheme_emitter_context::register_function_template_specialization(
    value tag, value type, value identifier)
{
  if (not has_template(tag))
    throw std::invalid_argument {std::format(
        "Register specialization for non-existent template {}", tag)};
  assert(m_templates.contains(tag) and
          "Template and its specialization must belong to the same context");
  const bool ok = m_specializations.emplace(type, identifier).second;
  assert(ok and "Failed to reigster template function specialization");
}


bool
opi::scheme_emitter_context::find_function_template_speciailization(
    value type, value &identifier) const noexcept
{
  const auto it = m_specializations.find(type);
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

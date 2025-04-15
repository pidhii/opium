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


struct scheme_emitter_context {
  scheme_emitter_context(const prolog &pl, const scheme_to_prolog &pl_emitter,
                         code_tape &output)
  : output {std::back_inserter(output)},
    pl {pl},
    prolog_emitter {pl_emitter},
    parent {*this}
  { }

  scheme_emitter_context(scheme_emitter_context &parent, code_tape &output)
  : output {std::back_inserter(output)},
    legal_types {parent.legal_types},
    pl {parent.pl},
    prolog_emitter {parent.prolog_emitter},
    parent {parent}
  { }

  bool
  has_parent() const noexcept
  { return &parent != this; }

  bool
  has_template(value tag) const noexcept
  {
    if (templates.contains(tag))
      return true;
    return has_parent() and parent.has_template(tag);
  }

  // TODO: move to cpp file
  const function_template
  find_template(value tag) const
  {
    const auto it = templates.find(tag);
    if (it != templates.end())
      return it->second;
    if (has_parent())
      return parent.find_template(tag);
    throw std::range_error {std::format("No template with tag {}", tag)};
  }

  // TODO: move to cpp file
  void
  register_template(value tag, const function_template &functemplate)
  {
    if (has_template(tag))
      throw std::invalid_argument {
          std::format("Duplica template definition (tag: {})", tag)};
    templates.emplace(tag, functemplate);
  }

  /**
   * Reigster specialization of a function template for future reuse
   *
   * \param tag Template tag
   * \param type Specialization type
   * \param identifier Identifier of the specialized function
   */
  void
  register_function_template_specialization(value tag, value type,
                                            value identifier)
  {
    if (not has_template(tag))
      throw std::invalid_argument {std::format(
          "Register specialization for non-existent template {}", tag)};
    assert(templates.contains(tag) and
           "Template and its specialization must belong to the same context");
    const bool ok = specializations.emplace(type, identifier).second;
    assert(ok and "Failed to reigster template function specialization");
  }

  bool
  find_function_template_speciailization(value type, value &identifier)
  {
    const auto it = specializations.find(type);
    if (it == specializations.end())
      return false;
    identifier = it->second;
    return true;
  }

  code_tape_output output; /**< Output tape for supplementary code */

  opi::stl::unordered_map<value /* code tag */, function_template>
      templates; /**< Template definitions */
  opi::stl::unordered_map<value /* type */, value /* function identifier */>
      specializations; /**< Cache for produced template specializations */
  opi::stl::unordered_set<value> legal_types = {"num", "nil", "str", "sym",
                                                "boolean"};

  const prolog &pl; /**< Storage for predicates */
  const scheme_to_prolog &prolog_emitter; /**< Storage for type info */

  scheme_emitter_context &parent;
}; // struct opi::scheme_emitter_context


} // namespace opi
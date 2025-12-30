#pragma once

#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/exceptions.hpp"


namespace opi::osl {


struct ambiguous_operator_use: bad_code {
  using bad_code::bad_code;
};

enum class operator_kind { sufix, infix, postfix };

struct operator_definition {
  value name;
  operator_kind kind;
  int precedence; // negative for left-to-right, positive for right-to-left
}; // struct osl::operator_defintion


struct operator_key {
  operator_kind kind;
  value name;

  operator_key(operator_kind kind, value name)
  : kind {kind}, name {name}
  { }

  operator_key(const operator_definition opdef)
  : kind {opdef.kind}, name {opdef.name}
  { }

  bool
  operator == (const operator_key &other) const noexcept
  { return kind == other.kind and name == other.name; }
};

struct operator_hash {
  size_t
  operator () (const operator_key &opkey) const noexcept
  {
    size_t hash = 0;
    hash_combine(hash, opkey.kind);
    hash_combine(hash, opkey.name);
    return hash;
  }
}; // struct osl::operator_hash

class operators_library {
  public:
  void
  add_operator(operator_kind kind, value name, int precedence)
  {
    operator_definition opdef {name, kind, precedence};
    m_operators.emplace(operator_key {opdef}, opdef);
  }

  bool
  find_operator(operator_kind kind, value name,
                operator_definition &result) const
  {
    const auto it = m_operators.find(operator_key {kind, name});
    if (it == m_operators.end())
      return false;
    result = it->second;
    return true;
  }

  private:
  stl::unordered_map<operator_key, operator_definition, operator_hash>
      m_operators;
}; // class osl::operators_library


value
resolve_operator_precedence(value expr, const operators_library &oplib);


} // namespace opi::osl
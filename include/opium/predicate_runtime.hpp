#pragma once

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/stl/vector.hpp"


namespace opi {


// Helper function to check if a value is a variable
inline bool
starts_with_capital(const char *str, [[maybe_unused]] size_t len)
{ return isupper(static_cast<unsigned char>(str[0])); }


[[gnu::pure]] inline bool
is_variable(value x)
{ return issym(x) and starts_with_capital(x->sym.data, x->sym.len); }


struct cell {
  enum class kind : unsigned char {
    variable,  // Represents a variable without a value
    value      // Represents a variable with a concrete value
  };

  cell(): next {this}, val {nil}, kind {kind::variable}, isdead {false} { }
  explicit cell(value v): next {this}, val {v}, kind {kind::value}, isdead {false} { }

  cell *next;
  value val;   // Only used when kind == kind::value
  kind kind;
  bool isdead; // Flag to undo logics via unlinking 'old' cells
};


class predicate_runtime {
  public:
  predicate_runtime() = default;

  predicate_runtime(const predicate_runtime&) = delete;
  void operator = (const predicate_runtime&) = delete;

  auto
  variables() const noexcept
  { return std::views::keys(m_varmap); }

  // Substitute variables available in this frame.
  value
  substitute_vars(value x) const;

  // Get the cell corresponding to the given variable
  cell*
  operator [] (value var);

  // Get cell with value `val`. Cell will be owned by this runtime and will thus
  // be marked by `mark_dead()`.
  cell*
  make_term(value val);

  // Find the representative cell for a variable
  cell*
  find(cell *x) const;

  // Get the value of a variable if it has one
  bool
  get_value(value var, value &result) const noexcept;

  bool
  has_value(value var) const noexcept
  { value _ = nil; return get_value(var, _); }

  // Check `a` and `b` for equality. Two symbols are considered equal
  // when represent identified variables.
  bool
  equal(value a, value b);

  // Unify two variables or a variable with a value
  //
  // Note asymmetry in arguments: LHS (`x`) will never be made a representative of
  // a chain. This guarantees preservation of hierarchy between LHS and RHS
  // variables and grants possibility to undo unifications by erasing RHS
  // variables from all link-chains.
  //
  // (sorry, not the best explanation, im not good at formulating my thoughs;
  //  in short it is required to make `mark_dead()` work)
  bool
  unify(cell *x, cell *y);

  // Assign a value to a variable
  bool
  assign(cell* x, value val)
  { return unify(x, make_term(val)); }

  // Will effectively erase unifications with variables in this frame.
  void
  mark_dead();

  private:
  opi::unordered_map<value, cell*> m_varmap;
  opi::vector<cell*> m_terms;
}; // class opi::predicate_runtime

} // namespace opi

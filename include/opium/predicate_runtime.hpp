#pragma once

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/deque.hpp"

#include <stdexcept>


namespace opi {


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


// Find the representative cell
cell *
find(cell *x);

// Unify two variables or a variable with a value
//
// Note asymmetry in arguments: LHS (`x`) will never be made a representative of
// a chain. This guarantees preservation of hierarchy between LHS and RHS
// variables and grants possibility to undo unifications by erasing RHS
// variables from all link-chains.
//
// (sorry, not the best explanation, im not good at formulating my thoughs;
//  in short it is required to make `mark_dead()` work)
//
// TODO: doesn't have to be a class method
bool
unify(cell *x, cell *y);

// Reconstruct value from a cell
value
reconstruct(cell *x);

value
reconstruct(value x);

bool
get_value(cell *x, value &result);


class predicate_runtime {
  public:
  struct error: public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

  predicate_runtime()
  : m_parent {nullptr},
    m_preduid {nullptr},
    m_signature {nil},
    m_prev_frame {nullptr}
  { }

  // Constructor with parent
  explicit predicate_runtime(predicate_runtime *parent)
  : m_parent {parent}, m_preduid {nullptr}, m_signature {nil},
    m_prev_frame {parent}
  { }

  // Disable copying
  predicate_runtime(const predicate_runtime&) = delete;
  void operator = (const predicate_runtime&) = delete;

  // Will mark this frame wtth `name` and `signature` if unique and return true;
  // otherwize -- when non-unique -- do nothing and return false.
  bool
  try_sign(const void *preduid, value signature,
           const predicate_runtime &prev) noexcept;

  // List visible variables
  std::ranges::range auto
  variables() const noexcept
  {
    opi::deque<value> result;
    for (const predicate_runtime *prt = this; prt; prt = prt->m_parent)
    {
      for (const auto &[key, _] : prt->m_varmap)
        result.push_back(key);
    }
    return result;
  }

  // Get the cell corresponding to the given variable
  cell*
  operator [] (value var);

  cell*
  operator [] (value var) const;

  // Get cell with value `val`. Cell will be owned by this runtime and will thus
  // be marked by `mark_dead()`.
  cell*
  make_term(value val);

  // Will effectively erase unifications with variables in this frame.
  void
  mark_dead();

  private:
  opi::unordered_map<value, cell *> m_varmap; // Associations table between
                                              // variables and cells
  opi::vector<cell *> m_terms; // Calues-cells created within this frame
  predicate_runtime *m_parent; // Parent runtime for variable lookup
  const void *m_preduid;       // Active predicate
  value m_signature; // Resolved signature of active predicate arguments to
                     // identify recursion
  const predicate_runtime *m_prev_frame;
}; // class opi::predicate_runtime


value
insert_cells(predicate_runtime &prt, value expr);


} // namespace opi

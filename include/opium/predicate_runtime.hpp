#pragma once

#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/deque.hpp"
#include "opium/format.hpp"

#include <concepts>
#include <stdexcept>
#include <stdexcept>


namespace opi {


// Cell representing a variable that can be identified with other variables or
// bound to some value
//
// Note: cells implement a DFS-like (Disjoint Forest Set) logic to represent
// identified variables
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
// Throws std::runtime_error on attempt to unify to bound cells.
bool
unify(cell *x, cell *y);


// Functoin accepting a cell pointer and returning a value
template <typename T>
concept unbound_variable_handler = requires(T & f)
{
  { f((cell *){}) } -> std::convertible_to<value>;
};


// Exceptioni type thrown upon encountering unbound variable by default
// reconstructors
struct reconstruction_error: public std::runtime_error {
  using std::runtime_error::runtime_error;
}; // strcut opi::reconstruction_error


// Default handle of unbound variables during reconstruction
//
// Note: throws reconstruction_error upon unbound variable
struct throw_on_unbound_variable {
  value
  operator () (cell *x) //const
  { throw reconstruction_error {std::format("unbound variable @ {}", (void*)x)}; }
}; // struct opi::throw_on_unbound_variable
static_assert(unbound_variable_handler<throw_on_unbound_variable>);


// Reconstruct value produced during prolog query
// 1) from an explicit cell
template <unbound_variable_handler UVHandle = throw_on_unbound_variable>
value
reconstruct(cell *x, UVHandle uvhandler = UVHandle {});
// 2) from expression containing nested cells
template <unbound_variable_handler UVHandle = throw_on_unbound_variable>
value
reconstruct(value x, UVHandle uvhandler = UVHandle {});


// Get value of a bound cell. Return true when the value was found; otherwize,
// return false and `result` argument is left unmodified
bool
get_value(cell *x, value &result);


class predicate_runtime;
template <typename T>
concept nonterminal_variable_handler =
    std::regular_invocable<T, predicate_runtime & /* roll-back PRT */,
                           cell * /* nonterminal cell */>;


struct ignore_nonterminal_variables {
  void
  operator () (predicate_runtime &, cell *) const noexcept { /* do nothing */ }
}; // struct opi::ignore_nonterminal_variables
static_assert(nonterminal_variable_handler<ignore_nonterminal_variables>);


struct assign_nonterminal_to {
  assign_nonterminal_to(value val);

  void
  operator () (predicate_runtime &rollbackprt, cell *x) const noexcept;

  private:
  value m_val;
}; // class opi::assign_nonterminal_to
static_assert(nonterminal_variable_handler<assign_nonterminal_to>);


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
  template <nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables>
  bool
  try_sign(const void *preduid, value signature, const predicate_runtime &prev,
           NTVHandler ntvhandler = NTVHandler {}) noexcept;

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


/**
 * Match arguments between predicate runtime environments
 * 
 * @param prt Predicate runtime environment
 * @param ert External runtime environment
 * @param pexpr Predicate expression
 * @param eexpr External expression
 * @return true if arguments match, false otherwise
 */
bool
match_arguments(predicate_runtime &prt, const predicate_runtime &ert,
                value pexpr, value eexpr);


value
insert_cells(predicate_runtime &prt, value expr);


} // namespace opi


#include "opium/predicate_runtime.inl"

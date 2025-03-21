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


/**
 * Cell representing a variable that can be identified with other variables or
 * bound to some value
 * 
 * \note Cells implement a DFS-like (Disjoint Forest Set) logic to represent
 * identified variables
 */
struct cell {
  enum class kind : unsigned char {
    variable,  /**< Represents a variable without a value */
    value      /**< Represents a variable with a concrete value */
  };

  cell(): next {this}, val {nil}, kind {kind::variable}, isdead {false} { }
  explicit cell(value v): next {this}, val {v}, kind {kind::value}, isdead {false} { }

  cell *next;
  value val;   // Only used when kind == kind::value
  kind kind;
  bool isdead; // Flag to undo logics via unlinking 'old' cells
};


/**
 * Find the representative cell
 * 
 * \param x The cell to find the representative for
 * \return Pointer to the representative cell
 */
cell *
find(cell *x);


/**
 * Unify two variables or a variable with a value
 * 
 * \note Asymmetry in arguments: LHS (`x`) will never be made a representative of
 * a chain. This guarantees preservation of hierarchy between LHS and RHS
 * variables and grants possibility to undo unifications by erasing RHS
 * variables from all link-chains.
 * 
 * (This is required to make `mark_dead()` work)
 * 
 * \param x Left-hand side cell
 * \param y Right-hand side cell
 * \return True if unification succeeded, false otherwise
 * \throws std::runtime_error On attempt to unify two bound cells
 */
bool
unify(cell *x, cell *y);


/**
 * Concept for a function accepting a cell pointer and returning a value
 */
template <typename T>
concept unbound_variable_handler = requires(T & f)
{
  { f((cell *){}) } -> std::convertible_to<value>;
};


/**
 * Exception type thrown upon encountering unbound variable by default
 * reconstructors
 */
struct reconstruction_error: public std::runtime_error {
  using std::runtime_error::runtime_error;
}; // strcut opi::reconstruction_error


/**
 * Default handler of unbound variables during reconstruction
 * 
 * \note Throws reconstruction_error upon encountering an unbound variable
 */
struct throw_on_unbound_variable {
  value
  operator () (cell *x) //const
  { throw reconstruction_error {std::format("unbound variable @ {}", (void*)x)}; }
}; // struct opi::throw_on_unbound_variable
static_assert(unbound_variable_handler<throw_on_unbound_variable>);


/**
 * Reconstruct value produced during prolog query from an explicit cell
 * 
 * \tparam UVHandle Type of unbound variable handler
 * \param x Cell to reconstruct from
 * \param uvhandler Handler for unbound variables
 * \return Reconstructed value
 */
template <unbound_variable_handler UVHandle = throw_on_unbound_variable>
value
reconstruct(cell *x, UVHandle uvhandler = UVHandle {});
/**
 * Reconstruct value produced during prolog query from expression containing nested cells
 * 
 * \tparam UVHandle Type of unbound variable handler
 * \param x Expression to reconstruct from
 * \param uvhandler Handler for unbound variables
 * \return Reconstructed value
 */
template <unbound_variable_handler UVHandle = throw_on_unbound_variable>
value
reconstruct(value x, UVHandle uvhandler = UVHandle {});


/**
 * Get value of a bound cell
 * 
 * \param x Cell to get value from
 * \param result Output parameter to store the value
 * \return True when the value was found; otherwise,
 * return false and `result` argument is left unmodified
 */
bool
get_value(cell *x, value &result);


class predicate_runtime;
template <typename T>
concept nonterminal_variable_handler =
    std::regular_invocable<T, predicate_runtime & /* roll-back PRT */,
                           cell * /* nonterminal cell */>;


/**
 * Handler that ignores nonterminal variables
 */
struct ignore_nonterminal_variables {
  /**
   * Do nothing
   */
  void
  operator () (predicate_runtime &, cell *) const noexcept { /* do nothing */ }
}; // struct opi::ignore_nonterminal_variables
static_assert(nonterminal_variable_handler<ignore_nonterminal_variables>);


/**
 * Handler that assigns a value to nonterminal variables
 */
struct assign_nonterminal_to {
  /**
   * Constructor
   * 
   * \param val Value to assign to nonterminal variables
   */
  assign_nonterminal_to(value val);

  /**
   * Assign the stored value to a nonterminal variable
   * 
   * \param rollbackprt Predicate runtime for rollback
   * \param x Nonterminal cell to assign to
   */
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

  /**
   * Constructor with parent
   * 
   * \param parent Parent predicate runtime
   */
  explicit predicate_runtime(predicate_runtime *parent)
  : m_parent {parent}, m_preduid {nullptr}, m_signature {nil},
    m_prev_frame {parent}
  { }

  /**
   * Disable copying
   */
  predicate_runtime(const predicate_runtime&) = delete;
  void operator = (const predicate_runtime&) = delete;

  /**
   * Will mark this frame with `preduid` and `signature` if unique and return true;
   * otherwise -- when non-unique -- do nothing and return false.
   * 
   * \tparam NTVHandler Type of nonterminal variable handler
   * \param preduid Predicate unique identifier
   * \param signature Resolved signature of active predicate arguments
   * \param prev Previous predicate runtime frame
   * \param ntvhandler Handler for nonterminal variables
   * \return True if frame was marked as unique, false otherwise
   */
  template <nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables>
  bool
  try_sign(const void *preduid, value signature, const predicate_runtime &prev,
           NTVHandler ntvhandler = NTVHandler {}) noexcept;

  /**
   * List visible variables
   * 
   * \return Range of visible variables
   */
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

  /**
   * Get the cell corresponding to the given variable
   * 
   * \param var Variable to get cell for
   * \return Pointer to the cell
   */
  cell*
  operator [] (value var);
  cell*
  operator [] (value var) const;

  /**
   * Get cell with value `val`
   * 
   * \note Cell will be owned by this runtime and will thus
   * be marked by `mark_dead()`.
   * 
   * \param val Value to create cell with
   * \return Pointer to the created cell
   */
  cell*
  make_term(value val);

  /**
   * Will effectively erase unifications with variables in this frame
   */
  void
  mark_dead();

  private:
  opi::unordered_map<value, cell *> m_varmap; /**< Associations table between variables and cells */
  opi::vector<cell *> m_terms; /**< Values-cells created within this frame */
  predicate_runtime *m_parent; /**< Parent runtime for variable lookup */
  const void *m_preduid;       /**< Active predicate */
  value m_signature; /**< Resolved signature of active predicate arguments to identify recursion */
  const predicate_runtime *m_prev_frame; /**< Previous frame in the chain */
}; // class opi::predicate_runtime


/**
 * Match arguments between predicate runtime environments
 * 
 * \param[in,out] prt Predicate runtime environment
 * \param[in,out] ert External runtime environment
 * \param pexpr Predicate expression
 * \param eexpr External expression
 * \return true if arguments match, false otherwise
 */
bool
match_arguments(predicate_runtime &prt, const predicate_runtime &ert,
                value pexpr, value eexpr);


/**
 * Insert cells into an expression
 * 
 * \param prt Predicate runtime environment
 * \param expr Expression to insert cells into
 * \return Modified expression with cells inserted
 */
value
insert_cells(predicate_runtime &prt, value expr);


} // namespace opi


#include "opium/predicate_runtime.inl"

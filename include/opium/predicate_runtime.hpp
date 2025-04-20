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

#include "opium/stl/unordered_set.hpp"
#include "opium/value.hpp"
#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/stl/unordered_map.hpp"
#include "opium/stl/vector.hpp"
#include "opium/format.hpp" // IWYU pragma: export
#include "opium/exceptions.hpp"

#include <concepts>

/**
 * \file predicate_runtime.hpp
 * Runtime environment for Prolog predicates
 * 
 * This file defines the runtime environment for Prolog predicates,
 * including variable binding and unification.
 * 
 * \ingroup prolog
 */


namespace opi {

constexpr char CELL[] = "#__Cell";

/**
 * Cell representing a variable that can be identified with other variables or
 * bound to some value
 * 
 * \note Cells implement a DFS-like (Disjoint Forest Set) logic to represent
 * identified variables
 * 
 * \ingroup prolog
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
 * 
 * \ingroup prolog
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
 * \throws bad_code On attempt to unify two bound cells
 * 
 * \ingroup prolog
 */
bool
unify(cell *x, cell *y);


/**
 * Concept for a function accepting a cell pointer and returning a value
 * 
 * \ingroup prolog
 */
template <typename T>
concept unbound_variable_handler = requires(T &f)
{
  { f((cell *){}) } -> std::convertible_to<value>;
};

/**
 * Exception type thrown upon encountering unbound variable by default
 * reconstructors
 * 
 * \ingroup prolog
 */
struct reconstruction_error: public bad_code {
  using bad_code::bad_code;
}; // strcut opi::reconstruction_error


/**
 * Default handler of unbound variables during reconstruction
 * 
 * \note Throws reconstruction_error upon encountering an unbound variable
 * 
 * \ingroup prolog
 */
struct throw_on_unbound_variable {
  value
  operator () (cell *x) const
  { throw reconstruction_error {std::format("unbound variable @ {}", (void*)x)}; }
}; // struct opi::throw_on_unbound_variable
static_assert(unbound_variable_handler<throw_on_unbound_variable>);


struct stringify_unbound_variables_t {
  value
  operator () (cell *x) const
  { return sym(std::format("X:{}", (void*)x)); }
}; // struct opi::stringify_unbound_variables
static_assert(unbound_variable_handler<stringify_unbound_variables_t>);
constexpr stringify_unbound_variables_t stringify_unbound_variables;


struct ignore_unbound_variables_t {
  value
  operator () (cell *x) const
  { return cons(CELL, ptr(x)); }
}; // struct opi::throw_on_unbound_variable
static_assert(unbound_variable_handler<ignore_unbound_variables_t>);
constexpr ignore_unbound_variables_t ignore_unbound_variables;


/**
 * Reconstruct value produced during prolog query from an explicit cell
 * 
 * \tparam UVHandle Type of unbound variable handler
 * \param x Cell to reconstruct from
 * \param uvhandler Handler for unbound variables
 * \return Reconstructed value
 * 
 * \ingroup prolog
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
 * 
 * \ingroup prolog
 */
bool
get_value(cell *x, value &result);


/**
 * Test two patterns with variables (cells) for equivalence
 * 
 * \ingroup prolog
 */
bool
equivalent(value x, value y);


class predicate_runtime;
template <typename T>
concept nonterminal_variable_handler =
    std::regular_invocable<T, predicate_runtime & /* roll-back PRT */,
                           cell * /* nonterminal cell */>;


/**
 * Handler that ignores nonterminal variables
 * 
 * \ingroup prolog
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
 * 
 * \ingroup prolog
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


/**
 * Runtime environment for Prolog predicates
 * 
 * \ingroup prolog
 */
class predicate_runtime {
  public:
  struct error: public bad_code {
    using bad_code::bad_code;
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
   * Move constructor
   * 
   * \param other Predicate runtime to move from
   */
  predicate_runtime(predicate_runtime &&other) noexcept
  : m_varmap(std::move(other.m_varmap)),
    m_terms(std::move(other.m_terms)),
    m_parent(other.m_parent),
    m_preduid(other.m_preduid),
    m_signature(other.m_signature),
    m_prev_frame(other.m_prev_frame)
  {
    // NOTE: it is an undefined behaviour if there will be any further operations
    // made to the `other`
  }

  ~predicate_runtime()
  { mark_dead(); }

  /**
   * Move assignment operator
   * 
   * \param other Predicate runtime to move from
   * \return Reference to this object
   */
  predicate_runtime &
  operator = (predicate_runtime &&other) noexcept
  {
    if (this != &other) {
      // First mark this object as dead to clean up any resources
      mark_dead();
      
      // Move resources from other
      m_varmap = std::move(other.m_varmap);
      m_terms = std::move(other.m_terms);
      m_parent = other.m_parent;
      m_preduid = other.m_preduid;
      m_signature = other.m_signature;
      m_prev_frame = other.m_prev_frame;
      
      // NOTE: it is an undefined behaviour if there will be any further
      // operations made to the `other`
    }
    return *this;
  }

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
    opi::stl::unordered_set<value> result;
    for (const predicate_runtime *prt = this; prt; prt = prt->m_parent)
    {
      for (const auto &[key, _] : prt->m_varmap)
        result.insert(key);
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
   * Make cell with value `val`
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
   * Make unbound cell
   * 
   * \note Cell will be owned by this runtime and will thus
   * be marked by `mark_dead()`.
   * 
   * \return Pointer to the created cell
   */
  cell*
  make_var();

  /**
   * Will effectively erase unifications with variables in this frame
   */
  void
  mark_dead();

  private:
  opi::stl::unordered_map<value, cell *> m_varmap; /**< Associations table between variables and cells */
  opi::stl::vector<cell *> m_terms; /**< Values-cells created within this frame */
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
 * 
 * \ingroup prolog
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
 * 
 * \ingroup prolog
 */
value
insert_cells(predicate_runtime &prt, value expr);


} // namespace opi


#include "opium/predicate_runtime.inl"

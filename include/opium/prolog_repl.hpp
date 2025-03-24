#pragma once

#include "opium/prolog.hpp"
#include "opium/value.hpp"

#include <stdexcept>

/**
 * \file prolog_repl.hpp
 * Prolog REPL (Read-Eval-Print Loop) implementation
 * 
 * This file defines the interactive REPL for the Prolog language implementation.
 * 
 * \ingroup prolog
 */


namespace opi {

/**
 * Prolog REPL (Read-Eval-Print Loop) implementation
 * 
 * Extends the base prolog evaluator with interactive capabilities
 * for defining predicates and executing queries
 * 
 * \ingroup prolog
 */
class prolog_repl: public prolog {
  public:
  /**
   * Error type for REPL-specific errors
   * 
   * \ingroup prolog
   */
  struct error: public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

  /**
   * Process an expression in the REPL
   * 
   * Handles predicate definitions and queries
   * 
   * \param expr Expression to process
   * \throws error If the expression is not understood
   */
  void
  operator << (value expr);

  private:
  /**
   * Execute a query and display the results
   * 
   * \param expr Query expression to execute
   */
  void
  _query(value expr);
}; // class opi::prolog_repl

} // namespace opi

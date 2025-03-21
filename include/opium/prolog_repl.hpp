#pragma once

#include "opium/prolog.hpp"
#include "opium/value.hpp"

#include <stdexcept>


namespace opi {

/**
 * Prolog REPL (Read-Eval-Print Loop) implementation
 * 
 * Extends the base prolog evaluator with interactive capabilities
 * for defining predicates and executing queries
 */
class prolog_repl: public prolog {
  public:
  /**
   * Error type for REPL-specific errors
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

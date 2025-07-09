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

#include "opium/prolog.hpp"
#include "opium/value.hpp"
#include "opium/exceptions.hpp"

#include <filesystem>

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
  struct error: public bad_code {
    using bad_code::bad_code;
  };

  void
  add_path_prefix(const std::filesystem::path &prefix) noexcept
  { m_path_prefixes.emplace_back(prefix); }

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

  std::set<std::filesystem::path> m_loaded_pathes;
  std::vector<std::filesystem::path> m_path_prefixes;
}; // class opi::prolog_repl

} // namespace opi

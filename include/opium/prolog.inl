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


/**
 * Template implementation of opium/prolog.hpp members 
 *
 * Used global flags:
 * - DebugQueryLocations: trace query by locations
 * - DebugQuery: trace expressions that query is going through
 * - DebugQueryVars: trace values of local variables during query
 * - DebugCall: print call goals
 * - DebugPredicateMatches: print information about successfult matches on predicates
 * - DebugSignatureClash: print information about predicate signature clashes
 * - DebugPredicateFailure: print information about failures to satisfy predicates
 * - DebugOrBranches: extra verbosity for flow of OR-expressions
 */
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/source_location.hpp"
#include "opium/pretty_print.hpp"
#include "opium/logging.hpp"
#include "opium/utilities/separate_stack_executor.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/value.hpp"

#include <functional>
#include <gc/gc.h>

#include <unistd.h>


namespace opi {

/**
 * Impelementation of some builtin prolog predicates
 */
namespace prolog_impl {

/**
 * Implementation of `var X` predicate
 *
 * Test if given value is an unbound variable.
 *
 * \param x Value to test
 * \return True if \p x is an unbound variable; false otherwize
 */
bool
var(value x);

/**
 * Implementation of `debug Args...` predicate
 *
 * \param expr Expression with location attached that triggered the call (optional)
 * \param args List with arguments of the predicate
 */
void
debug(value expr, value args);

/**
 * Implementation of `elements-of L` predicate
 *
 * Collect elements of (possibly infinite) list.
 *
 * \param l List whos elements to collect
 * \return Elements of the list \p l
 */
value
elements_of(value l);

} // namespace opi::prolog_impl


inline std::ranges::view auto
prolog::predicate_branches(const std::string &name) const
{
  const auto it = m_db.find(name);
  // NOTE Commenting the following lines out implies change of semantics:
  //      before) Missing predicate is an error
  //      after) Missing predicate is query failure
  // if (it == m_db.end())
  //   throw error {std::format("No such predicate: {}", name)};

  return std::ranges::subrange(it, m_db.end()) |
         std::views::take(m_db.count(name)) |
         std::views::values;
}


inline predicate_runtime
_create_derived_prt(predicate_runtime &parent)
{
  predicate_runtime derived {&parent};
  for (const value var : parent.variables())
  {
    [[maybe_unused]] const bool ok = unify(parent[var], derived[var]);
    assert(ok and "Failed to create variable in or-clause");
  }
  return derived;
}


static value
_remove_dynamic_function_dispatch_body(value x)
{
  if (ispair(x))
  {
    if (car(x) == "#dynamic-function-dispatch")
      return list(car(x), car(cdr(x)), car(cdr(cdr(x))), car(cdr(cdr(cdr(x)))));
    else
      return cons(_remove_dynamic_function_dispatch_body(car(x)),
                  _remove_dynamic_function_dispatch_body(cdr(x)));
  }
  else
    return x;
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont,
                  NTVHandler ntvhandler) const
{
  if (e == list("and"))
  {
    cont();
    return;
  }

  utl::state_saver _ {m_depth};
  m_depth ++;

  // Track the last evaluated expression. This is an ad-hoc method to provide
  // user with information on the source of failure during the TypeCheck. 
  _update_last_expr(m_depth, e);

  // NOTE: This Prolog interpreter is implemented as simple AST evaluator with
  // CPS without proper tail-calls. Consequently evaluation of every single
  // expression consumes space on the stack without releasing it untill the
  // unwind reaches this expression. Consequently, it runs out of the stack when
  // presented with significantly big code. The (hopefuly temprary) workaround
  // is to dynamically allocate and use new stacks whenever depth of recursion
  // reaches its threshold (i.e. close to running out of stack).
  //
  // The depth-based thresholds below are obtained by trial-and-error (FIXME)
#ifdef OPIUM_RELEASE_BUILD
  if (m_depth % 5000 == 0)
#else
  // Non-release build produces much larger frames during function calls, so it
  // consumes stack faster
  if (m_depth % 1000 == 0)
#endif
  {
    warning("switching stack (depth = {})", m_depth);
    bool i_disabled_gc = false;
    if (not GC_is_disabled())
    {
      warning("disabling GC");
      GC_disable();
      i_disabled_gc = true;
    }

    utl::guarded_stack stack {4000};
    warning("jumping onto new stack");
    utl::separate_stack_executor exec {stack.stack_pointer(), stack.size()};
    exec(std::bind(&prolog::make_true<Cont, NTVHandler>, this, std::ref(ert), e,
                   cont, ntvhandler));

    if (i_disabled_gc)
    {
      warning("switching back GC");
      GC_enable();
    }

    return;
  }

#ifndef OPIUM_RELEASE_BUILD
  if (loglevel >= loglevel::debug)
  {
    std::ostringstream message;

    // Show source location whenever possible
    source_location location;
    if (global_flags.contains("DebugQueryLocations") and get_location(e, location))
      message << display_location(location, 2, "\e[1m", "\e[2m");

    // Optionaly show Prolog sources
    if (global_flags.contains("DebugQuery"))
    {
      const value erec = reconstruct(e, stringify_unbound_variables);
      const value edisp = _remove_dynamic_function_dispatch_body(erec);
      message << "\nProlog expr: " << pprint_pl(edisp, 13);
    }

    // Optionaly show current state of local variables
    if (global_flags.contains("DebugQueryVars"))
    {
      message << "\nwhere\n";
      for (const value var : ert.variables()) 
      {
        const value val = reconstruct(ert[var], stringify_unbound_variables);
        message << std::format("  {} = {}\n", var, val);
      }
    }

    // Display the message
    const std::string messagestr = message.str();
    if (not messagestr.empty())
      debug("make_true {}", messagestr);
  }
#endif

  switch (tag(e))
  {
    case tag::pair: {
      if (issym(car(e), "if"))
      {
        const value cond = car(cdr(e));
        const value thenbr = car(cdr(cdr(e)));
        const value elsebr = car(cdr(cdr(cdr(e))));
        return _make_if_true(ert, cond, thenbr, elsebr, cont, ntvhandler);
      }
      else if (issym(car(e), "insert-cells"))
      {
        // Validate the form
        if (length(cdr(e)) != 2)
          throw bad_code {std::format("Invalid expression: {}", e), e};

        // Get the arguments
        const value expr = car(cdr(e));
        const value result = car(cdr(cdr(e)));

        // Reconstruct expr before inserting cells because `insert_cells()` does
        // not follow cell-binds itself
        const value recoexpr = reconstruct(expr, ignore_unbound_variables);

        // Insert cells
        // NOTE: use separate runtime to separate variable names scope
        predicate_runtime tempns;
        const value resultexpr = insert_cells(tempns, recoexpr);

        // Bind result and continue
        make_true(ert, list("=", resultexpr, result), cont, ntvhandler);

        // Roll back
        tempns.mark_dead();

        return;
      }
      else if (issym(car(e), "debug"))
      {
        prolog_impl::debug(e, cdr(e));
        return cont();
      }
      else if (issym(car(e), "and"))
      {
        const value clauses = cdr(e);
        return _make_and_true(ert, clauses, cont, ntvhandler);
      }
      else if (issym(car(e), "or"))
      {
        const value clauses = cdr(e);
        return _make_or_true(ert, clauses, cont, ntvhandler);
      }
      else if (issym(car(e), "var"))
      {
        if (prolog_impl::var(car(cdr(e))))
          cont();
        return;
      }
      else if (issym(car(e), "nonvar"))
      {
        if (not prolog_impl::var(car(cdr(e))))
          cont();
        return;
      }
      else if (issym(car(e), "=@="))
      {
        const value a = car(cdr(e));
        const value b = car(cdr(cdr(e)));
        if (equivalent(a, b))
          cont();
        return;
      }
      else if (issym(car(e), "elements-of"))
      {
        const value l = reconstruct(car(cdr(e)), ignore_unbound_variables);
        if (ispair(l) and issym(car(l), CELL))
          throw error {"Can't invoke `elements-of` with unbound variable", l};
        const value result = car(cdr(cdr(e)));
        const value elements = prolog_impl::elements_of(l);
        return make_true(ert, list("=", result, elements), cont, ntvhandler);
      }
      else if (issym(car(e), "call"))
      {
        value goal = car(cdr(e));
        if (prolog_impl::var(goal))
        {
          throw error {
              std::format("Can't use unbound variable as a Goal\nin {}",
                          pprint_pl(reconstruct(e, ignore_unbound_variables))),
              e};
        }

        // Reconstruct "Goal" as much as possible
        goal = reconstruct(goal, ignore_unbound_variables);
        if (global_flags.contains("DebugCall"))
          debug("call Goal: {}", goal);

        if (ispair(goal))
          e = append(goal, cdr(cdr(e)));
        else
          e = cons(goal, cdr(cdr(e)));

        // Run new goal
        return make_true(ert, e, cont, ntvhandler);
      }
      else if (issym(car(e), "not"))
      {
        bool success = false;
        std::function<void()> newcont = [&]() { success = true; };
        make_true(ert, car(cdr(e)), newcont, ntvhandler);
        if (not success)
          cont();
        return;
      }
      else if (issym(car(e)))
      {
        const std::string predname = car(e)->sym.data;
        const value eargs = cdr(e);
        bool broke_through = false;
        std::function<void()> newcont = [&broke_through, &cont]() {
          broke_through = true;
          cont();
        };

        try {
          for (const predicate &p : predicate_branches(predname))
            _make_predicate_true(ert, p, eargs, newcont, ntvhandler);
        }
        catch (const error &exn)
        {
          opi::error("{}", exn.display());
          throw error {std::format("unhandled exception",
                                   reconstruct(e, ignore_unbound_variables)),
                       e};
        }

        // Couldn't satisfy a predicate
        if (not broke_through and global_flags.contains("DebugPredicateFailure"))
        {
          std::ostringstream what;
          what << "\e[38;5;1mfailed\e[0m to satisfy predicate\n";
          source_location location;
          if (get_location(e, location))
            what << display_location(location, 1);
          debug("[{:5}] {}", m_depth, what.str());
        }
        return;
      }
      break;
    }

    case tag::boolean:
      if (e->boolean)
        cont();
      return;

    default:;
  }

  throw error {std::format("Invalid expression: {}", e), e};
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::_make_if_true(predicate_runtime &ert, value cond, value thenbr,
                      value elsebr, Cont cont, NTVHandler ntvhandler) const
{
  // Create auxiliary derived predicate_runtime
  predicate_runtime crt = _create_derived_prt(ert);

  // Try <cond> -> <then>
  bool isthen = false;
  std::function<void()> thencont = [&]() {
    isthen = true;
    make_true(crt, thenbr, cont, ntvhandler);
  };
  make_true(crt, cond, thencont, ntvhandler);
  crt.mark_dead();

  // Otherwize, go <else>
  if (not isthen)
    make_true(ert, elsebr, cont, ntvhandler);
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::_make_and_true(predicate_runtime &ert, value clauses, Cont cont,
                       NTVHandler ntvhandler) const
{
  // Sequentially process all clauses until none left; no clauses <=> true
  if (ispair(clauses))
  {
    // Separate head clause
    const value head = car(clauses);
    const value tail = cdr(clauses);
    std::function<void()> andcont = [&]() {
      _make_and_true(ert, tail, cont, ntvhandler);
    };
    // Make head true and then proceed with other clauses
    make_true(ert, head, andcont, ntvhandler);
  }
  else
    cont();
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::_make_or_true(predicate_runtime &ert, value clauses, Cont cont,
                      NTVHandler ntvhandler) const
{
  if (global_flags.contains("DebugOrBranches"))
    debug("looping over OR-branches");
  // indent _ {};
  const std::string hrule (30, '/');
  for ([[maybe_unused]] int cnt = 0; const value clause : range(clauses))
  {
#ifndef OPIUM_RELEASE_BUILD
    if (loglevel >= loglevel::debug and global_flags.contains("DebugOrBranches"))
    {
      std::ostringstream buf;
      buf << std::format("\n\e[38;5;4;1m{} BRANCH {}\e[0m\n", hrule, hrule);
      for (int i = 0; const value clause : range(clauses))
      {
        buf << std::format("{} {}\n", i++ == cnt ? "\e[38;5;4;1m->\e[0m" : "-", clause);
        source_location location;
        if (get_location(clause, location))
          buf << display_location(location, 2, i-1 == cnt ? "\e[38;5;4;1m" : "", "\e[2m");
      }
      debug("{}\n", buf.str());
      cnt ++;
    }
#endif

    // Create auxiliary derived predicate_runtime
    predicate_runtime crt = _create_derived_prt(ert);

    make_true(crt, clause, cont, ntvhandler);
    crt.mark_dead();
  }
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::_make_predicate_true(predicate_runtime &ert, const predicate &pred,
                             value eargs, Cont cont,
                             NTVHandler ntvhandler) const
{
  predicate_runtime prt;

  const value predicate_arguments = cdr(pred.signature());
  const value pargs = insert_cells(prt, predicate_arguments);

  if (match_arguments(prt, pargs, eargs))
  {
    const value signature = eargs;

    const value body = insert_cells(prt, pred.body());

#ifndef OPIUM_RELEASE_BUILD
    if (global_flags.contains("DebugPredicateMatches"))
    {
      debug("\e[38;5;2mmatch\e[0m on {}{}", pred.name(),
            reconstruct(signature, stringify_unbound_variables));
    }
#endif

    if (prt.try_sign(&pred, signature, ert, ntvhandler))
    {
      make_true(prt, body, cont, ntvhandler);
    }
    else
    {
#ifndef OPIUM_RELEASE_BUILD
      if (loglevel >= loglevel::debug)
      { // Messaage about signature clash
        std::ostringstream message;
        message << "\e[38;5;3msignature clash\e[0m";
        // Optionaly show current state of local variables
        if (global_flags.contains("DebugSignatureClash"))
        {
          message << "\nwhere\n";
          message << "  signature: "
                  << reconstruct(signature, stringify_unbound_variables)
                  << "\n";
          for (const value var : ert.variables())
          {
            const value val = reconstruct(ert[var], stringify_unbound_variables);
            message << std::format("  {} = {}\n", var, val);
          }
        }
        // Display the message
        debug("{}", message.str());
      }
#endif

      cont();
    }
  }
#ifndef OPIUM_RELEASE_BUILD
  else if (global_flags.contains("DebugPredicateMismatches"))
  {
    debug("\e[38;5;1mno match\e[0m on {}{}\npredicate: {}{}", pred.name(),
          reconstruct(eargs, stringify_unbound_variables),
          pred.name(), reconstruct(pargs, stringify_unbound_variables));
  }
#endif
  // Undo constraints introduced by match_arguments()
  prt.mark_dead();
}


} // namespace opi

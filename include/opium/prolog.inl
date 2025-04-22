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
 */
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/source_location.hpp"
#include "opium/pretty_print.hpp"
#include "opium/logging.hpp"

#include <functional>


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
  if (it == m_db.end())
    throw error {std::format("No such predicate: {}", name)};

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
    const bool ok = unify(parent[var], derived[var]);
    assert(ok and "Failed to create variable in or-clause");
  }
  return derived;
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont,
                  NTVHandler ntvhandler) const
{
  if (loglevel >= loglevel::debug)
  {
    std::ostringstream message;

    // Show source location whenever possible
    source_location location;
    if (get_location(e, location))
      message << display_location(location, 2, "\e[1m", "\e[2m");

    // Optionaly show Prolog sources
    if (global_flags.contains("DebugQuery"))
      message << "\nProlog expr: "
              << pprint_pl(reconstruct(e, stringify_unbound_variables), 13);

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

  switch (e->t)
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
        predicate_runtime tmpprt {&ert};
        const value resultexpr = insert_cells(tmpprt, recoexpr);

        // Bind result and continue
        make_true(tmpprt, list("=", resultexpr, result), cont, ntvhandler);

        // Clean up the temporary runtime
        tmpprt.mark_dead();

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
        if (l->t == tag::pair and issym(car(l), CELL))
          throw error {"Can't invoke `elements-of` with unbound variable", l};
        const value result = car(cdr(cdr(e)));
        const value elements = prolog_impl::elements_of(l);
        return make_true(ert, list("=", result, elements), cont, ntvhandler);
      }
      else if (issym(car(e), "query"))
      { // FIXME: there seem to be bugs related to preservation of bindings
        // TODO: move to a separate function

        // Gather arguments
        value goal = car(cdr(e));
        const value result = car(cdr(cdr(e)));

        // Create auxiliary derived predicate_runtime
        predicate_runtime crt = _create_derived_prt(ert);

        // Prepare continuation that will be accumulating query results
        cell *tmp = crt.make_var();
        const value tmpvar = cons(CELL, ptr(tmp));
        value acc = nil;
        std::function<void()> query = [&]() {
          const value tmpval = reconstruct(tmp, [&](cell *c) {
            cell *newcell = ert.make_var();
            unify(c, newcell);
            return cons(CELL, ptr(newcell));
          });
          acc = cons(tmpval, acc);
        };

        // Insert placeholder variable that would hold query results in the goal
        if (goal->t == tag::pair)
          goal = append(goal, list(tmpvar));
        else
          goal = list(goal, tmpvar);
        debug("query goal: {}", reconstruct(goal, stringify_unbound_variables));

        // Run the query
        make_true(crt, goal, query, ntvhandler);
        crt.mark_dead();
        
        // Bind accumulated results with result-argument
        const value bindexpr = list("=", result, acc);
        make_true(ert, bindexpr, cont, ntvhandler);

        return;
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
        debug("call Goal: {}", goal);

        if (goal->t == tag::pair)
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
          throw error {std::format("{}\nin expression {}", exn.what(),
                                   reconstruct(e, ignore_unbound_variables)),
                       e};
        }

        // Couldn't satisfy a predicate
        if (not broke_through)
          debug("\e[38;5;1mfailed\e[0m to satisfy predicate {}",
                reconstruct(e, stringify_unbound_variables));
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
  if (clauses->t == tag::pair)
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
  debug("looping over OR-branches");
  indent _ {};
  const std::string hrule (30, '/');
  for (int cnt = 0; const value clause : range(clauses))
  {
    std::ostringstream buf;
    buf << std::format("\n\e[38;5;4;1m{} BRANCH {}\e[0m\n", hrule, hrule);
    for (int i = 0; const value expr : range(clauses))
    {
      buf << std::format("{} {}\n", i++ == cnt ? "\e[38;5;4;1m->\e[0m" : "-", expr);
      source_location location;
      if (get_location(expr, location))
        buf << display_location(location, 2, i-1 == cnt ? "\e[38;5;4;1m" : "", "\e[2m");
    }
    debug("{}\n", buf.str());
    cnt ++;

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

  const value pargs = insert_cells(prt, list(pred.arguments()));

  if (match_arguments(prt, ert, pargs, eargs))
  {
    const value signature = eargs;

    const value body = insert_cells(prt, pred.body());
    // debug("make predicate true:\n{}{} :-\n  {}", pred.name(),
    //       reconstruct(pargs, stringify_unbound_variables),
    //       pprint_pl(reconstruct(body, stringify_unbound_variables), 2));

    debug("\e[38;5;2mmatch\e[0m on {}{}", pred.name(),
          reconstruct(signature, stringify_unbound_variables));

    if (prt.try_sign(&pred, signature, ert, ntvhandler))
    {
      make_true(prt, body, cont, ntvhandler);
    }
    else
    {
      if (loglevel >= loglevel::debug)
      { // Messaage about signature clash
        std::ostringstream message;
        message << "\e[38;5;3msignature clash\e[0m";
        // Optionaly show current state of local variables
        if (global_flags.contains("make_true:debug-signature-clash"))
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

      cont();
    }
  }
  // Undo constraints introduced by match_arguments()
  prt.mark_dead();
}


} // namespace opi

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
#include "opium/pretty_print.hpp"
#include "opium/logging.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/utilities/separate_stack_executor.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/value.hpp"

#include <gc/gc.h>


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


static value
_remove_dynamic_function_dispatch_body(value x)
{
  if (ispair(x))
  {
    if (car(x) == "#dynamic-function-dispatch")
      return list(list_ref(x, 0),  // tag
                  list_ref(x, 1),  // name
                  list_ref(x, 2),  // args
                  list_ref(x, 3)); // results
    else
      return cons(_remove_dynamic_function_dispatch_body(car(x)),
                  _remove_dynamic_function_dispatch_body(cdr(x)));
  }
  else
    return x;
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::make_true(value e, Cont cont, NTVHandler ntvhandler, Guide guide) const
{
  const call_frame rootframe {nullptr, nil, nullptr, std::nullopt};
  _make_true(rootframe, e, cont, ntvhandler, guide);
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::_make_true(const call_frame &frame, value e, Cont cont,
                   NTVHandler ntvhandler, Guide guide) const
{
  // Update trace.
  _trace_expr(e);

  // Follow the guide
  if constexpr (not std::is_same_v<Guide, bruteforce_query>)
  {
    if (not guide(e, m_trace))
      return;
  }

  if (ispair(e) and ISSYM(car(e), "and") and is(cdr(e), nil))
  {
    cont();
    return;
  }

  utl::state_saver _ {m_depth, m_trace};
  m_depth ++;

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
  if (m_depth % 7000 == 0)
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

    utl::guarded_stack stack {2000};
    warning("jumping onto new stack");
    utl::separate_stack_executor exec {stack.stack_pointer(), stack.size()};
    exec(std::bind(&prolog::_make_true<Cont, NTVHandler, Guide>, this,
                   std::cref(frame), e, cont, ntvhandler, guide));

    if (i_disabled_gc)
    {
      warning("switching back GC");
      GC_enable();
    }

    return;
  }

  switch (tag(e))
  {
    case tag::pair: {
      if (ISSYM(car(e), "if"))
      {
        const value cond = car(cdr(e));
        const value thenbr = car(cdr(cdr(e)));
        const value elsebr = car(cdr(cdr(cdr(e))));
        return _make_if_true(frame, cond, thenbr, elsebr, cont, ntvhandler, guide);
      }
      else if (ISSYM(car(e), "insert-cells"))
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
        _make_true(frame, list("=", resultexpr, result), cont, ntvhandler, guide);

        return;
      }
      else if (ISSYM(car(e), "debug"))
      {
        prolog_impl::debug(e, cdr(e));
        return cont();
      }
      else if (ISSYM(car(e), "and"))
      {
        const value clauses = cdr(e);
        return _make_and_true(frame, clauses, cont, ntvhandler, guide);
      }
      else if (ISSYM(car(e), "or"))
      {
        const value clauses = cdr(e);
        return _make_or_true(frame, clauses, cont, ntvhandler, guide);
      }
      else if (ISSYM(car(e), "var"))
      {
        if (prolog_impl::var(car(cdr(e))))
          cont();
        return;
      }
      else if (ISSYM(car(e), "nonvar"))
      {
        if (not prolog_impl::var(car(cdr(e))))
          cont();
        return;
      }
      else if (ISSYM(car(e), "=@="))
      {
        const value a = car(cdr(e));
        const value b = car(cdr(cdr(e)));
        if (equivalent(a, b))
          cont();
        return;
      }
      else if (ISSYM(car(e), "elements-of"))
      {
        const value l = reconstruct(car(cdr(e)), ignore_unbound_variables);
        if (ispair(l) and is(car(l), cell_tag))
          throw error {"Can't invoke `elements-of` with unbound variable", l};
        const value result = car(cdr(cdr(e)));
        const value elements = prolog_impl::elements_of(l);
        return _make_true(frame, list("=", result, elements), cont, ntvhandler, guide);
      }
      else if (ISSYM(car(e), "call"))
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
        #ifndef OPIUM_RELEASE_BUILD
        if (global_flags.contains("DebugCall"))
          debug("call Goal: {}", goal);
        #endif

        if (ispair(goal))
          e = append(goal, cdr(cdr(e)));
        else
          e = cons(goal, cdr(cdr(e)));

        // Run new goal
        return _make_true(frame, e, cont, ntvhandler, guide);
      }
      else if (ISSYM(car(e), "not"))
      {
        bool success = false;
        std::function<void()> newcont = [&]() { success = true; };
        _make_true(frame, car(cdr(e)), newcont, ntvhandler, guide);
        if (not success)
          cont();
        return;
      }
      else if (issym(car(e)))
      {
        const std::string predname = sym_name(car(e)).data();
        const value eargs = cdr(e);

        utl::state_saver _ {m_cutpred};
        for (const predicate &p : predicate_branches(predname))
        {
          m_cutpred = false;
          _make_predicate_true(frame, p, eargs, cont, ntvhandler, guide);
          if (m_cutpred or m_cut)
            break;
        }
        return;
      }
      break;
    }

    case tag::boolean:
      if (e->boolean)
        cont();
      return;

    case tag::sym:
      if (ISSYM(e, "!") or ISSYM(e, "cut-choice"))
      {
        cont();
        m_cut = true;
        return;
      }
      else if (ISSYM(e, "cut-predicate-choice"))
      {
        cont();
        m_cutpred = true;
        return;
      }

    default:;
  }

  throw error {std::format("Invalid expression: {}", e), e};
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::_make_if_true(const call_frame &frame, value cond, value thenbr,
                      value elsebr, Cont cont, NTVHandler ntvhandler,
                      Guide guide) const
{
  // Try <cond> -> <then>
  bool isthen = false;
  std::function<void()> thencont = [&]() {
    isthen = true;
    _make_true(frame, thenbr, cont, ntvhandler, guide);
  };
  _make_true(frame, cond, thencont, ntvhandler, guide);

  // Otherwize, go <else>
  if (not isthen)
    _make_true(frame, elsebr, cont, ntvhandler, guide);
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::_make_and_true(const call_frame &frame, value clauses, Cont cont,
                       NTVHandler ntvhandler, Guide guide) const
{
  // Sequentially process all clauses until none left; no clauses <=> true
  if (ispair(clauses))
  {
    // Separate head clause
    const value head = car(clauses);
    const value tail = cdr(clauses);
    std::function<void()> andcont = [&]() {
      _make_and_true(frame, tail, cont, ntvhandler, guide);
    };
    // Make head true and then proceed with other clauses
    _make_true(frame, head, andcont, ntvhandler, guide);
  }
  else
    cont();
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::_make_or_true(const call_frame &frame, value clauses, Cont cont,
                      NTVHandler ntvhandler, Guide guide) const
{
  for (const value clause : range(clauses))
  {
    _make_true(frame, clause, cont, ntvhandler, guide);
    if (m_cut)
      break;
  }
}


// This has to be equivalent to result of composition of 
// signature -> reconstruct[stringify_unbound_variables]
//           -> _remove_dynamic_function_dispatch_body
//           -> insert_cells
// (unless i fucked it up)
//
// NOTE: Removal of dyn-dispatch bodies seems to be unnecessary (which is good);
//       however, it has significant (positive) impact on performance.
//
// TODO: I think this function can be made tail-callable (and so do many other
//       similar functions)
static value
_make_signature_snapshot(value s,
                         stl::unordered_map<const object *, value> &argmem,
                         stl::unordered_map<const cell *, value> &cellmem)
{
  const auto it = argmem.find(&*s);
  if (it != argmem.end())
    return it->second;

  if (ispair(s))
  {
    if (car(s) == cell_tag)
    {
      cell *repr = find(static_cast<cell*>(cdr(s)->ptr));
      value val;
      if (get_value(repr, val))
        return argmem[&*s] = _make_signature_snapshot(val, argmem, cellmem);
      else
      {
        const auto it = cellmem.find(repr);
        if (it != cellmem.end())
          return it->second;
        else
          return cellmem[repr] = cons(cell_tag, ptr(make<cell>()));
      }
    }
    else if (car(s) == "#dynamic-function-dispatch")
    {
      return argmem[&*s] = list(_make_signature_snapshot(list_ref(s, 0), argmem, cellmem),  // tag
                                _make_signature_snapshot(list_ref(s, 1), argmem, cellmem),  // name
                                _make_signature_snapshot(list_ref(s, 2), argmem, cellmem),  // args
                                _make_signature_snapshot(list_ref(s, 3), argmem, cellmem)); // results
    }
    else
    {
      value &result = argmem.emplace(&*s, cons(nil, nil)).first->second;
      set_car(result, _make_signature_snapshot(car(s), argmem, cellmem));
      set_cdr(result, _make_signature_snapshot(cdr(s), argmem, cellmem));
      return result;
    }
  }
  else
    return argmem[&*s] = s;
}

inline value
_make_signature_snapshot(value signature)
{
  stl::unordered_map<const object *, value> argmem;
  stl::unordered_map<const cell *, value> cellmem;
  return _make_signature_snapshot(signature, argmem, cellmem);
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler,
          prolog_guide Guide>
void
prolog::_make_predicate_true(const call_frame &frame, const predicate &pred,
                             value eargs, Cont cont, NTVHandler ntvhandler,
                             Guide guide) const
{
  predicate_runtime mprt;
  const value predicate_arguments = cdr(pred.signature());
  const value pargs = insert_cells(mprt, predicate_arguments);

  if (match_arguments(mprt, pargs, eargs))
  {
    const value body = insert_cells(mprt, pred.body());
    const value signature = _make_signature_snapshot(eargs);
    if (not _try_recursion_heuristic(frame, &pred, signature, cont, ntvhandler))
    {
      call_frame newframe;
      _add_call_frame(&pred, signature, frame, newframe);
      _make_true(newframe, body, cont, ntvhandler, guide);
    }
  }
}


template <nonterminal_variable_handler NTVHandler,
          prolog_continuation Continuation>
bool
opi::prolog::_try_recursion_heuristic(const call_frame &frame, const void *id,
                                      value signature, Continuation &&cont,
                                      NTVHandler ntvhandler) const
{
  OPI_FUNCTION_BENCHMARK

  bool applied_heuristic = false;
  for (const call_frame *framep = &frame; framep != nullptr; framep = framep->prev)
  {
    if (id == framep->id)
    {
      assert(id != nullptr);
      predicate_runtime prt;
      if (equivalent(signature, framep->signature))
      {
        if (not match_arguments(prt, signature, framep->signature))
          throw std::logic_error {"failed to match on equivalent patterns"};

        // Process non-terminal variables
        if constexpr (not std::is_same_v<NTVHandler, ignore_nonterminal_variables>)
        {
          // NOTE: I dont know if potential permutation made here are not
          // violating the flow of logics
          for (const value var : prt.variables())
          {
            // Variables present in signature but not bound by `match_arguments`
            // are regarded as non-terminal (computation of their type will not
            // terminate).
            // Use `reconstruct` to scan for (possibly) nested unbound variables
            // variables and trigger user-handler (`ntvhandler`) on each of them.
            reconstruct(prt[var], [&](cell *x) {
              ntvhandler(prt, x);
              return nil;
            });
          }
        }

        applied_heuristic = true;
        cont();
      }
    }
  }

  // No clash encountered.
  return applied_heuristic;
}


} // namespace opi

/**
 * Template implementation of opium/prolog.hpp members 
 */
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/pretty_print.hpp"
#include "opium/logging.hpp"

#include <functional>


namespace opi {


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


static inline bool
_var_implementation(value x)
{
  value _ = nil;
  if (x->t == tag::pair and issym(car(x), "__cell"))
    return not get_value(static_cast<cell*>(cdr(x)->ptr), _);
  return false;
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont,
                  NTVHandler ntvhandler) const
{
  if (loglevel >= loglevel::debug)
  {
    std::ostringstream message;
    message << "make_true ";

    // Show source location whenever possible
    bool haslocation = false;
    source_location location;
    if (lisp_parser::get_location(e, location))
    {
      haslocation = true;
      message << display_location(location, 2, "\e[1m", "\e[2m");
    }

    // Optionaly show Prolog sources
    if (not haslocation or global_flags.contains("make_true:debug-prolog"))
      message << "\nProlog expr: "
              << pprint_pl(reconstruct(e, stringify_unbound_variables), 13);

    // Optionaly show current state of local variables
    if (global_flags.contains("make_true:debug-variables"))
    {
      message << "\nwhere\n";
      for (const value var : ert.variables()) 
      {
        const value val = reconstruct(ert[var], stringify_unbound_variables);
        message << std::format("  {} = {}\n", var, val);
      }
    }

    // Display the message
    debug("{}", message.str());
  }

  switch (e->t)
  {
    case tag::pair: {
      if (issym(car(e), "if"))
      { // TODO move to separate function
        const value cond = car(cdr(e));
        const value thenbr = car(cdr(cdr(e)));
        const value elsebr = car(cdr(cdr(cdr(e))));

        // Create auxiliary predicate_runtime with parent reference to the
        // current frame
        predicate_runtime crt {&ert};

        // Unify variables from parent frame with ones in the current frame
        for (const value var : ert.variables())
        {
          const bool ok = unify(ert[var], crt[var]);
          assert(ok and "Failed to create variable in or-clause");
        }

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

        return;
      }
      else if (issym(car(e), "debug"))
      {
        std::clog << "debug ";
        source_location location;
        if (lisp_parser::get_location(e, location))
          std::clog << display_location(location, 0, "\e[38;5;5;1m", "\e[2m");
        for (const value x : range(cdr(e)))
          display(std::clog, reconstruct(x, stringify_unbound_variables));
        std::clog << std::endl;
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
        if (_var_implementation(car(cdr(e))))
          cont();
        return;
      }
      else if (issym(car(e), "nonvar"))
      {
        if (not _var_implementation(car(cdr(e))))
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
      else if (issym(car(e), "call"))
      {
        // Reconstruct "Goal" as much as possible
        const value predicate =
            reconstruct(car(cdr(e)), ignore_unbound_variables);

        if (predicate->t == tag::pair)
          // TODO verify that car(predicate) is symbol (?)
          e = append(predicate, cdr(cdr(e)));
        else
          e = cons(predicate, cdr(cdr(e)));

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
        for (const predicate &p : predicate_branches(predname))
          _make_predicate_true(ert, p, eargs, newcont, ntvhandler);

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

  // TODO: add location to the exception if available
  throw error {std::format("Invalid expression: {}", e)};
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
  for (const value clause : range(clauses))
  {
    // Create auxiliary predicate_runtime with parent reference to the current frame
    predicate_runtime crt {&ert};
    
    // Unify variables from parent frame with ones in the current frame
    for (const value var : ert.variables())
    {
      const bool ok = unify(ert[var], crt[var]);
      assert(ok and "Failed to create variable in or-clause");
    }
    
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
    debug("\e[38;5;2mmatch\e[0m on {}{}", pred.name(),
          reconstruct(signature, stringify_unbound_variables));
    if (prt.try_sign(&pred, signature, ert, ntvhandler))
    {
      make_true(prt, insert_cells(prt, pred.body()), cont, ntvhandler);
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
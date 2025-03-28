/**
 * Template implementation of opium/prolog.hpp members 
 */
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"

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


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont,
                  NTVHandler ntvhandler) const
{
  debug("make_true {}", reconstruct(e, stringify_unbound_variables));

  indent _ {};
  switch (e->t)
  {
    case tag::pair: {
      if (issym(car(e), "and"))
      {
        const value clauses = cdr(e);
        return _make_and_true(ert, clauses, cont, ntvhandler);
      }
      else if (issym(car(e), "or"))
      {
        const value clauses = cdr(e);
        return _make_or_true(ert, clauses, cont, ntvhandler);
      }
      else if (issym(car(e)))
      {
        const std::string predname = car(e)->sym.data;
        const value eargs = cdr(e);
        for (const predicate &p : predicate_branches(predname))
          _make_predicate_true(ert, p, eargs, cont, ntvhandler);
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

  throw error {std::format("Invalid expression: {}", e)};
}


template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::_make_and_true(predicate_runtime &ert, value clauses, Cont cont,
                       NTVHandler ntvhandler) const
{
    // Case 1: sequentially process and-clauses
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
  // Case 2: no clauses left <=> true
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
    predicate_runtime crt(&ert);
    
    // Unify variables from parent frame with ones in the current frame
    for (const value var : ert.variables())
    {
      const bool ok = unify(ert[var], crt[var]);
      assert(ok and "Failed to create variable in or-clause");
    }
    
    debug("[or] make_true({}) and <cont>", reconstruct(clause, stringify_unbound_variables));
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
  debug("match {} on {}{} :- {}",
        reconstruct(eargs, stringify_unbound_variables), pred.name(),
        reconstruct(pargs, stringify_unbound_variables), pred.body());

  if (match_arguments(prt, ert, pargs, eargs))
  {
    const value signature = eargs;
    debug("\e[38;5;2maccept\e[0m [signature={}{}]", pred.name(),
          reconstruct(signature, stringify_unbound_variables));
    indent _ {};
    if (prt.try_sign(&pred, signature, ert, ntvhandler))
    {
      debug("signed PRT");
      make_true(prt, insert_cells(prt, pred.body()), cont, ntvhandler);
    }
    else
    {
      debug("\e[38;5;3msignature clash\e[0m");
      cont();
    }
  }
  else
    debug("\e[38;5;1mreject\e[0m");
  // Undo constraints introduced by match_arguments()
  prt.mark_dead();
}


} // namespace opi
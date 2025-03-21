#pragma once

#include "opium/value.hpp"
#include "opium/format.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"
#include "opium/logging.hpp"

#include <asm-generic/errno.h>
#include <concepts>
#include <functional>
#include <string>
#include <cassert>
#include <ranges>
#


namespace opi {


// Predicate representation
class predicate {
  public:
  // TODO: validate types
  predicate(value sig, value body)
  : m_name {opi::car(sig)->sym.data},
    m_body {body}
  {
    for (opi::value x : opi::range(opi::cdr(sig)))
      m_args.emplace_back(x);
    debug("new predicate: {}{} :- {}", m_name, opi::list(m_args), m_body);
  }

  // Get name of the predicate
  const std::string&
  name() const noexcept
  { return m_name; }

  // Get predicate arguments (as std::range)
  auto
  arguments() const noexcept
  { return m_args; }

  // Get predicate body/rule
  value
  body() const noexcept
  { return m_body; }

  private:
  std::string m_name;
  opi::vector<value> m_args;
  value m_body;
}; // class opi::predicate


template <typename Cont>
concept prolog_continuation = std::regular_invocable<Cont>;


// Prolog evaluator
class prolog {
  public:
  struct error: public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

  // TODO: validate types
  const predicate&
  add_predicate(value sig, value body);

  auto
  predicate_branches(const std::string &name) const;

  template <
    prolog_continuation Cont,
    nonterminal_variable_handler NTVHandler = ignore_nonterminal_variables>
  void
  make_true(predicate_runtime &ert, value e, Cont cont,
            NTVHandler ntvhandler = NTVHandler {}) const;

  private:
  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_and_true(predicate_runtime &ert, value clauses, Cont cont,
                 NTVHandler ntvhandler) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_or_true(predicate_runtime &ert, value clauses, Cont cont,
                NTVHandler ntvhandler) const;

  template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
  void
  _make_predicate_true(predicate_runtime &ert, const predicate &pred,
                       value eargs, Cont cont, NTVHandler ntvhandler) const;

  private:
  opi::unordered_multimap<std::string, predicate> m_db;
}; // class opi::prolog


////////////////////////////////////////////////////////////////////////////////
//
//                     Template implementations
//
inline auto
prolog::predicate_branches(const std::string &name) const
{
  const auto it = m_db.find(name);
  if (it == m_db.end())
    throw error {std::format("No such predicate: {}", name)};

  return std::ranges::subrange(it, m_db.end()) |
         std::views::take(m_db.count(name));
}

template <prolog_continuation Cont, nonterminal_variable_handler NTVHandler>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont,
                  NTVHandler ntvhandler) const
{
  debug("make_true {}", e);

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
        for (const auto &[_, p] : predicate_branches(predname))
          _make_predicate_true(ert, p, eargs, cont, ntvhandler);
        return;
      }
      break;
    }

    case tag::boolean: {
      if (e->boolean)
        cont();
      return;
    }

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
    
    debug("[or] make_true({}) and <cont>", clause);
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
  debug("match {} on {}{} :- {}", eargs, pred.name(), pargs, pred.body());

  if (match_arguments(prt, ert, pargs, eargs))
  {
    const value signature = eargs;
    debug("\e[38;5;2maccept\e[0m [signature={}{}]", pred.name(), signature);
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

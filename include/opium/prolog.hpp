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
#include <set>


namespace opi {

bool
match_arguments(predicate_runtime &prt, const predicate_runtime &ert,
                value pexpr, value eexpr);


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
    debug("new predicate: ", m_name, opi::list(m_args), " :- ", m_body);
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

  // Get all predicate names defined in the database
  std::ranges::range auto
  predicate_names() const;

  template <prolog_continuation Cont>
  void
  make_true(predicate_runtime &ert, value e, Cont cont) const;

  private:
  template <prolog_continuation Cont>
  void
  _make_and_true(predicate_runtime &ert, value clauses, Cont cont) const;

  template <prolog_continuation Cont>
  void
  _make_or_true(predicate_runtime &ert, value clauses, Cont cont) const;

  template <prolog_continuation Cont>
  void
  _make_predicate_true(predicate_runtime &ert, const predicate &pred,
                       value eargs, Cont cont) const;

  private:
  opi::unordered_multimap<std::string, predicate> m_db;
}; // class opi::prolog


////////////////////////////////////////////////////////////////////////////////
//
//                     Template implementations
//
inline std::ranges::range auto
opi::prolog::predicate_names() const
{
  std::set<std::string> unique_names; // Use set to avoid duplicates

  // Iterate through all predicates in the database
  for (const std::string &name : m_db | std::views::keys)
    unique_names.emplace(name);

  return unique_names;
}

inline auto
prolog::predicate_branches(const std::string &name) const
{
  const auto it = m_db.find(name);
  if (it == m_db.end())
    throw error {opi::format("No such predicate: ", name)};

  return std::ranges::subrange(it, m_db.end()) |
         std::views::take(m_db.count(name));
}


template <prolog_continuation Cont>
void
prolog::make_true(predicate_runtime &ert, value e, Cont cont) const
{
  debug("make_true ", reconstruct(e));

  indent _ {};
  switch (e->t)
  {
    case tag::pair: {
      if (issym(car(e), "and"))
      {
        const value clauses = cdr(e);
        return _make_and_true(ert, clauses, cont);
      }
      else if (issym(car(e), "or"))
      {
        const value clauses = cdr(e);
        return _make_or_true(ert, clauses, cont);
      }
      else if (issym(car(e)))
      {
        const std::string predname = car(e)->sym.data;
        const value eargs = cdr(e);
        for (const auto &[_, p] : predicate_branches(predname))
          _make_predicate_true(ert, p, eargs, cont);
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

  throw error {format("Invalid expression: ", e)};
}


template <prolog_continuation Cont>
void
prolog::_make_and_true(predicate_runtime &ert, value clauses, Cont cont) const
{
    // Case 1: sequentially process and-clauses
  if (clauses->t == tag::pair)
  {
    // Separate head clause
    const value head = car(clauses);
    const value tail = cdr(clauses);
    std::function<void()> andcont = [&]() { _make_and_true(ert, tail, cont); };
    // Make head true and then proceed with other clauses
    make_true(ert, head, andcont);
  }
  // Case 2: no clauses left <=> true
  else
    cont();
}


template <prolog_continuation Cont>
void
prolog::_make_or_true(predicate_runtime &ert, value clauses, Cont cont) const
{
  for (const value clause : range(clauses))
  {
    // Create auxiliary predicate_runtime with parent reference to the current frame
    predicate_runtime crt(&ert);
    
    // Unify variables from parent frame with ones in the current frame
    for (const value var : ert.variables())
    {
      const bool ok = unify(ert[var], crt[var]);
      assert(ok && "Failed to create variable in or-clause");
    }
    
    debug("[or] make_true(", clause, ") and <cont>");
    make_true(crt, clause, cont);
    crt.mark_dead();
  }
}


template <prolog_continuation Cont>
void
prolog::_make_predicate_true(predicate_runtime &ert, const predicate &pred,
                             value eargs, Cont cont) const
{
  predicate_runtime prt;

  const value pargs = insert_cells(prt, list(pred.arguments()));
  debug("match ", reconstruct(eargs), " on ", pred.name(), reconstruct(pargs),
        " :- ", pred.body());

  if (match_arguments(prt, ert, pargs, eargs))
  {
    const value signature = eargs;
    debug("\e[38;5;2maccept\e[0m [signature=", pred.name(), signature, "]");
    indent _ {};
    if (prt.try_sign(&pred, signature, ert))
    {
      debug("signed PRT");
      std::function<void()> newcont =
          [&]() { cont(); };
      make_true(prt, insert_cells(prt, pred.body()), newcont);
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

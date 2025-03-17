#include "opium/value.hpp"
#include "opium/format.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/stl/vector.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/logging.hpp"

#include <concepts>
#include <functional>
#include <ios>
#include <iostream>
#include <cassert>
#include <cctype>
#include <ranges>
#include <utility>
#include <vector>


namespace opi {

bool
match_arguments(predicate_runtime &prt, predicate_runtime &ert, value pexpr,
                value eexpr)
{
  // Expand variables whenever possible
  if (is_variable(eexpr) and ert.get_value(eexpr, eexpr))
    return match_arguments(prt, ert, pexpr, eexpr);
  if (is_variable(pexpr) and prt.get_value(pexpr, pexpr))
    return match_arguments(prt, ert, pexpr, eexpr);

  if (is_variable(eexpr))
  {
    if (is_variable(pexpr))
      return prt.unify(prt[pexpr], ert[eexpr]);
    else
      return prt.assign(ert[eexpr], pexpr);
  }
  else if (is_variable(pexpr))
    return prt.assign(prt[pexpr], eexpr);

  // Structural match
  if (pexpr->t != eexpr->t)
    return false;

  switch (pexpr->t)
  {
    case tag::pair:
      return match_arguments(prt, ert, car(pexpr), car(eexpr)) and
             match_arguments(prt, ert, cdr(pexpr), cdr(eexpr));

    default:
      return equal(pexpr, eexpr);
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//                            Prolog program
//
class predicate {
  public:
  // TODO: validate types
  predicate(value sig, value body)
  : m_name {car(sig)->sym.data},
    m_body {body}
  {
    for (value x : range(cdr(sig)))
      m_args.emplace_back(x);
    std::cout << "predicate: " << m_name << list(m_args) << " :- " << m_body << std::endl;
  }

  // Get name of the predicate
  const std::string&
  name() const noexcept
  { return m_name; }

  // Get predicate arguments (as std::range)
  auto
  arguments() const noexcept
  { return m_args; }

  value
  body() const noexcept
  { return m_body; }

  private:
  std::string m_name;
  opi::vector<value> m_args;
  value m_body;
}; // class opi::predicate


class prolog {
  public:
  // TODO: validate types
  void
  add_predicate(value sig, value body)
  {
    m_db.emplace(std::piecewise_construct,
                 std::forward_as_tuple(car(sig)->sym.data),
                 std::forward_as_tuple(sig, body));
  }

  auto
  predicate_branches(const std::string &name) const
  {
    return std::ranges::subrange(m_db.find(name), m_db.end()) |
           std::views::take(m_db.count(name));
  }

  template <typename Cont> requires std::regular_invocable<Cont> void
  make_true(predicate_runtime &prt, value expr, Cont cont) const;

  private:
  template <typename Cont> requires std::regular_invocable<Cont> void
  _make_predicate_true(predicate_runtime &ert, const predicate &pred,
                       value eargs, Cont cont) const;

  private:
  opi::unordered_multimap<std::string, predicate> m_db;
}; // class opi::predicate


template <typename Cont> requires std::regular_invocable<Cont> void
prolog::make_true(predicate_runtime &ert, value e, Cont cont) const
{
  debug("make_true ", e);
  indent _ {};
  switch (e->t)
  {
    case tag::pair:
    {
      if (not issym(car(e)))
        throw std::runtime_error {
          format("expression must start with a symbol: ", car(e))};

      if (issym(car(e), "or"))
      {
        assert(!"unimplemented");
      }
      else if (issym(car(e), "and"))
      {
        const value clauses = cdr(e);
        // Case 1: sequentially process and-clauses
        if (clauses->t == tag::pair)
        {
          // Separate head clause
          const value head = car(clauses);
          const value tail = cdr(clauses);
          std::function<void()> andcont = [&] () {
            make_true(ert, cons(car(e), tail), cont);
          };
          // Make head true and then proceed with other clauses
          make_true(ert, head, andcont);
        }
        // Case 2: no clauses left <=> true
        else
          cont();
      }
      else
      {
        const value eargs = cdr(e);
        for (const auto &[_, p] : predicate_branches(car(e)->str.data))
          _make_predicate_true(ert, p, eargs, cont);
      }
      break;
    }

    case tag::boolean:
    {
      if (e->boolean)
        cont();
      break;
    }

    default:
      error("make_true unimplemented for ", e);
      std::terminate();
  }
}

template <typename Cont> requires std::regular_invocable<Cont> void
prolog::_make_predicate_true(predicate_runtime &ert, const predicate &pred,
                             value eargs, Cont cont) const
{
  const value pargs = list(pred.arguments());
  debug("match ", eargs, " [=", ert.substitute_vars(eargs), "]", " on ",
        pred.name(), list(pred.arguments()), " :- ", pred.body());

  predicate_runtime prt;
  if (match_arguments(prt, ert, pargs, eargs))
  {
    debug("\e[38;5;2maccept\e[0m");
    indent _{};
    make_true(prt, pred.body(), cont);
  }
  else
    debug("\e[38;5;1mreject\e[0m");
  // Undo constraints introduced by match_arguments()
  prt.mark_dead();
}

}; // namespace opi


#include <fstream>

int
main([[maybe_unused]] int argc, char **argv)
{
  using namespace opi;

  std::ifstream infile {argv[1], std::ios::binary};

  prolog pl;
  lisp_parser parser;
  opi::vector<value> queries;

  const auto tokens = parser.tokenize(infile);
  size_t cursor = 0;
  while (cursor < tokens.size())
  {
    const value expr = parser.parse_tokens(tokens, cursor);
    if (issym(car(expr), "predicate"))
      pl.add_predicate(car(cdr(expr)), cons(sym("and"), cdr(cdr(expr))));
    else if (issym(car(expr), "query"))
      queries.push_back(car(cdr(expr)));
    else
      throw std::runtime_error {format("undefined expression: ", expr)};
  }

  for (const value query : queries)
  {
    predicate_runtime prt;
    std::cout << "?- " << query << std::endl;
    pl.make_true(prt, query, [&]() {
      std::cout << "=> yes" << std::endl;
      for (const value var : prt.variables())
      {
        value val = nil;
        if (prt.get_value(var, val))
          std::cout << " " << var << " = " << val << std::endl;
        else
          std::cout << " " << var << " = ?" << std::endl;
      }
    });
  }
}


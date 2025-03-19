#include "opium/prolog.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"
#include <regex.h>


// Check if expression represents a cell (i.e. `(__cell . <pointer>)`) and
// return the cell pointer if it is.
static inline bool
_is_cell(opi::value expr, opi::cell *&result)
{
  if (expr->t == opi::tag::pair and opi::issym(car(expr), "__cell"))
  {
    result = static_cast<opi::cell*>(expr->cdr->ptr);
    return true;
  }
  return false;
}

static bool
_match_arguments(opi::predicate_runtime &prt, const opi::predicate_runtime &ert,
                 opi::value pexpr, opi::value eexpr, opi::value mem)
{
  opi::cell *c1, *c2;

  // Expand variables whenever possible
  if (_is_cell(eexpr, c1) and get_value(c1, eexpr))
    return memq(eexpr, mem) or
           _match_arguments(prt, ert, pexpr, eexpr, cons(eexpr, mem));
  if (_is_cell(pexpr, c1) and get_value(c1, pexpr))
    return memq(pexpr, mem) or
           _match_arguments(prt, ert, pexpr, eexpr, cons(pexpr, mem));

  // Unify or assign variables
  if (_is_cell(eexpr, c1))
  {
    if (_is_cell(pexpr, c2))
      return unify(c2, c1);
    else
      return unify(c1, prt.make_term(pexpr));
  }
  else if (_is_cell(pexpr, c1))
    return unify(c1, prt.make_term(eexpr));

  // Structural equality
  if (pexpr->t != eexpr->t)
    return false;

  switch (pexpr->t)
  {
    case opi::tag::pair:
      return match_arguments(prt, ert, car(pexpr), car(eexpr)) and
             match_arguments(prt, ert, cdr(pexpr), cdr(eexpr));

    default:
      return equal(pexpr, eexpr);
  }
}

bool
opi::match_arguments(opi::predicate_runtime &prt, const opi::predicate_runtime &ert,
                 opi::value pexpr, opi::value eexpr)
{ return _match_arguments(prt, ert, pexpr, eexpr, nil); }

const opi::predicate&
opi::prolog::add_predicate(opi::value sig, opi::value body)
{
  return m_db
      .emplace(std::piecewise_construct,
               std::forward_as_tuple(car(sig)->sym.data),
               std::forward_as_tuple(sig, body))
      ->second;
}

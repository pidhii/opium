#include "opium/prolog.hpp"


bool
opi::match_arguments(opi::predicate_runtime &prt, opi::predicate_runtime &ert,
                     opi::value pexpr, opi::value eexpr)
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


void
opi::prolog::add_predicate(opi::value sig, opi::value body)
{
  m_db.emplace(std::piecewise_construct,
               std::forward_as_tuple(car(sig)->sym.data),
               std::forward_as_tuple(sig, body));
}

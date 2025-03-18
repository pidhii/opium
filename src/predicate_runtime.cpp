#include "opium/predicate_runtime.hpp"

#include <cstring>
#include <ranges>


opi::cell*
opi::predicate_runtime::operator [] (value var)
{
  auto it = m_varmap.find(var);
  if (it == m_varmap.end())
    it = m_varmap.emplace(var, make<cell>()).first;
  return it->second;
}


opi::cell*
opi::predicate_runtime::make_term(value val)
{
  cell *valcell = make<cell>(val);
  m_terms.push_back(valcell);
  return valcell;
}


opi::cell*
opi::predicate_runtime::find(cell *x) const
{
  // Path compression: make all nodes in the path point to the root
  // if (x->next != x)
  //   x->next = find(x->next);
  // return x->next;

  // No path optimization to preserve exact relational structure
  while (x->next != x)
  {
    // If next cell in the chain is 'dead' cut the link making current cell a
    // root
    if (x->next->isdead)
      x->next = x;

    x = x->next;
  }
  return x;
}


bool
opi::predicate_runtime::get_value(value var, value &result) const noexcept
{
  auto it = m_varmap.find(var);
  if (it == m_varmap.end())
  {
    // If variable not found in current runtime, check parent if available
    if (m_parent != nullptr)
      return m_parent->get_value(var, result);
    return false;
  }

  cell *rep = find(it->second);
  if (rep->kind == cell::kind::value)
  {
    result = rep->val;
    return true;
  }
  return false;
}


bool
opi::predicate_runtime::equal(value a, value b)
{
  // If both are variables, check if they are unified
  if (is_variable(a) and is_variable(b))
    return find((*this)[a]) == find((*this)[b]);

  // Substitute variables
  if (is_variable(a))
    get_value(a, a);
  if (is_variable(b))
    get_value(b, b);

  // Check structural equality
  if (a->t != b->t)
    return false;
  switch (a->t)
  {
    case tag::sym:
      return a->sym.len == b->sym.len and
             std::strncmp(a->sym.data, b->sym.data, a->sym.len) == 0;

    case tag::nil:
      return true;

    case tag::num:
      return a->num == b->num;

    case tag::str:
      return a->str.len == b->str.len and
             std::strncmp(a->str.data, b->str.data, a->str.len) == 0;

    case tag::pair:
      return equal(car(a), car(b)) and
             equal(cdr(a), cdr(b));

    case tag::boolean:
      return a->boolean == b->boolean;
  }

  return false;
}


bool
opi::predicate_runtime::unify(cell *x, cell *y)
{
  cell* repx = find(x);
  cell* repy = find(y);

  // If they're already unified, nothing to do
  if (repx == repy)
    return true;

  // Case 1: Both are values
  if (repx->kind == cell::kind::value and repy->kind == cell::kind::value)
    throw error {"Can't unify two values"};

  // Case 2: x is a value, y is a variable
  if (repx->kind == cell::kind::value)
  {
    repy->next = repx;
    return true;
  }

  // Case 3: x is a variable, y is a value
  if (repy->kind == cell::kind::value)
  {
    repx->next = y;
    return true;
  }

  // Case 4: Both are variables
  repx->next = y;
  return true;
}


void
opi::predicate_runtime::mark_dead()
{
  for (cell *cell : m_varmap | std::views::values)
    cell->isdead = true;
  for (cell *cell : m_terms)
    cell->isdead = true;
}


opi::value
opi::predicate_runtime::substitute_vars(value x) const
{
  if (is_variable(x))
  {
    get_value(x, x);
    return x;
  }
  else if (x->t == tag::pair)
    return cons(substitute_vars(car(x)), substitute_vars(cdr(x)));
  else
    return x;
}

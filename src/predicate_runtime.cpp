#include "opium/predicate_runtime.hpp"
#include "opium/hash.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/prolog.hpp"

#include <cstring>
#include <ranges>


// Helper function to check if a value is a variable
static inline bool
_starts_with_capital(const char *str, [[maybe_unused]] size_t len)
{ return isupper(static_cast<unsigned char>(str[0])); }


[[gnu::pure]] static inline bool
_is_variable(opi::value x)
{ return opi::issym(x) and _starts_with_capital(x->sym.data, x->sym.len); }



opi::value
opi::insert_cells(predicate_runtime &prt, value expr)
{
  if (_is_variable(expr))
    return opi::cons(opi::sym("__cell"), opi::ptr(prt[expr]));
  else if (expr->t == opi::tag::pair)
    return opi::cons(insert_cells(prt, car(expr)),
                     insert_cells(prt, cdr(expr)));
  else
    return expr;
}


opi::cell *
opi::find(cell *x)
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
opi::unify(cell *x, cell *y)
{
  cell* repx = find(x);
  cell* repy = find(y);

  // If they're already unified, nothing to do
  if (repx == repy)
    return true;

  // Case 1: Both are values
  if (repx->kind == cell::kind::value and repy->kind == cell::kind::value)
    throw std::runtime_error {"Can't unify two values"};

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


bool
opi::get_value(cell *x, value &result)
{
  x = find(x);
  if (x->kind == cell::kind::value)
  {
    result = x->val;
    return true;
  }
  return false;
}


opi::cell*
opi::predicate_runtime::operator [] (value var)
{
  auto it = m_varmap.find(var);
  if (it == m_varmap.end())
    it = m_varmap.emplace(var, make<cell>()).first;
  return it->second;
}


opi::cell*
opi::predicate_runtime::operator [] (value var) const
{
  auto it = m_varmap.find(var);
  if (it == m_varmap.end())
    return make<cell>();
  else
    return it->second;
}


opi::cell*
opi::predicate_runtime::make_term(value val)
{
  cell *valcell = make<cell>(val);
  m_terms.push_back(valcell);
  return valcell;
}


void
opi::predicate_runtime::mark_dead()
{
  for (cell *cell : m_varmap | std::views::values)
    cell->isdead = true;
  for (cell *cell : m_terms)
    cell->isdead = true;
}


bool
opi::predicate_runtime::try_sign(const void *preduid, value signature,
                                 const predicate_runtime &prev) noexcept
{
  for (const predicate_runtime *prt = &prev; prt; prt = prt->m_prev_frame)
  {
    if (preduid == prt->m_preduid)
    {
      assert(prt->m_preduid != nullptr);
      const bool issimilar =
          match_arguments(*this, *prt, signature, prt->m_signature);
      debug("compare {} vs {} -> {}", signature, prt->m_signature, issimilar);
      if (issimilar)
        return false;
      mark_dead();
      m_varmap.clear();
    }
  }

  m_preduid = preduid;
  m_signature = signature;
  m_prev_frame = &prev;
  return true;
}
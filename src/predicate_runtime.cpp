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


#include "opium/predicate_runtime.hpp"
#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/source_location.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/value.hpp"

#include <cstring>


/**
 * Check if a value is a variable (symbol starting with a capital letter)
 * 
 * \param x Value to check
 * \param is_wildcard[out] Whether its a wildcard variable
 * \return True if the value is a variable
 */
static inline bool
_is_variable(opi::value x, bool &is_wildcard)
{
  if (not issym(x))
    return (is_wildcard = false);
  const bool starts_with_capital = isupper((unsigned char)x->sym.data[0]);
  const bool starts_with_underscore = x->sym.data[0] == '_';
  is_wildcard = starts_with_underscore and x->sym.len == 1;
  return starts_with_capital or starts_with_underscore;
}

/**
 * Check if a value is a quotation form
 * 
 * \param x Value to check
 * \return True if the value is a quotation form (quote, quasiquote, unquote, unquote-splicing)
 */
static inline bool
_is_quotation_form(opi::value x, const char* form_name)
{
  return x->t == opi::tag::pair && opi::issym(opi::car(x), form_name);
}

static opi::value
_insert_cells(opi::predicate_runtime &prt, opi::value expr,
              opi::stl::unordered_map<opi::object *, opi::value> &mem,
              int quasiquote_level = 0)
{
  const auto it = mem.find(&*expr);
  if (it != mem.end())
    return it->second;

  if (_is_quotation_form(expr, "quasiquote"))
  {
    // Validate expression
    if (length(cdr(expr)) != 1)
      throw opi::bad_code {
          std::format("Invalid quasiquote expression: {}", expr), expr};

    if (quasiquote_level == 0)
    {
      const opi::value result = _insert_cells(prt, car(cdr(expr)), mem, 1);
      mem.emplace(&*expr, result);
      copy_location(expr, result);
      return result;
    }
    else
    {
      opi::value result = cons("quasiquote", opi::nil);
      mem.emplace(&*expr, result);
      result->cdr = &*_insert_cells(prt, cdr(expr), mem, quasiquote_level + 1);
      copy_location(expr, result);
      return result;
    }
  }

  if (_is_quotation_form(expr, "unquote"))
  {
    // Validate expression
    if (length(cdr(expr)) != 1)
      throw opi::bad_code {
          std::format("Invalid unquote expression: {}", expr), expr};
    if (quasiquote_level == 0)
      throw opi::bad_code {
          std::format("Invalid unquote outside quasiquote: {}", expr), expr};

    if (quasiquote_level == 1)
    { // Fully evaluate the argument
      const opi::value result = _insert_cells(prt, car(cdr(expr)), mem, 0);
      mem.emplace(&*expr, result);
      copy_location(expr, result);
      return result;
    }
    else
    { // Drop one quasiquote level but otherwize preserve the datum
      opi::value result = cons("unquote", opi::nil);
      mem.emplace(&*expr, result);
      result->cdr = &*_insert_cells(prt, cdr(expr), mem, quasiquote_level - 1);
      copy_location(expr, result);
      return result;
    }
  }

  if (_is_quotation_form(expr, "unquote-splicing"))
    throw opi::bad_code {std::format("Unimplemented syntax: {}", expr), expr};

  if (_is_quotation_form(expr, "quote"))
  {
    // Validate expression
    if (length(cdr(expr)) != 1)
      throw opi::bad_code {
          std::format("Invalid quasiquote expression: {}", expr), expr};

    if (quasiquote_level == 0)
    {
      const opi::value result = car(cdr(expr));
      copy_location(expr, result);
    }
    else
      return expr;
  }

  opi::value result = opi::nil;
  bool iswild;
  if (quasiquote_level == 0 and _is_variable(expr, iswild))
    result = opi::cons(opi::CELL, opi::ptr(iswild ? prt.make_var() : prt[expr]));
  else if (expr->t == opi::tag::pair)
  {
    result = cons(opi::nil, opi::nil);
    mem.emplace(&*expr, result);
    result->car = &*_insert_cells(prt, car(expr), mem, quasiquote_level);
    result->cdr = &*_insert_cells(prt, cdr(expr), mem, quasiquote_level);
    copy_location(expr, result);
  }
  else
    result = expr;
    
  copy_location(expr, result);
  mem.emplace(&*expr, result);
  return result;
}

opi::value
opi::insert_cells(predicate_runtime &prt, value expr)
{
  opi::stl::unordered_map<object *, value> mem;
  return _insert_cells(prt, expr, mem, 0);
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


opi::cell*
opi::predicate_runtime::make_var()
{
  cell *varcell = make<cell>();
  m_terms.push_back(varcell);
  return varcell;
}



void
opi::predicate_runtime::mark_dead()
{
  for (cell *cell : m_varmap | std::views::values)
    cell->isdead = true;
  for (cell *cell : m_terms)
    cell->isdead = true;
}


/**
 * Check if expression represents a cell (i.e. `(CELL . <pointer>)`) and
 * return the cell pointer if it is
 * 
 * \param expr Expression to check
 * \param result Output parameter to store the cell pointer
 * \return True if the expression represents a cell, false otherwise
 */
static inline bool
_is_cell(opi::value expr, opi::cell *&result)
{
  if (expr->t == opi::tag::pair and opi::issym(opi::car(expr), opi::CELL) and
      opi::cdr(expr)->t == opi::tag::ptr)
  {
    result = static_cast<opi::cell*>(opi::cdr(expr)->ptr);
    return true;
  }
  return false;
}


static bool
_is_tag(opi::value x)
{ return opi::issym(x) and opi::sym_name(x)[0] == '#'; }

static bool
_match_arguments(opi::predicate_runtime &prt, const opi::predicate_runtime &ert,
                 opi::value pexpr, opi::value eexpr, opi::value mem)
{
  opi::cell *c1, *c2;

  // Test if was already called with these arguments and return imediately if so
  const opi::value argspair = cons(pexpr, eexpr);
  if (member(argspair, mem))
    return true;
  // Otherwize, remember the argument pair
  mem = cons(argspair, mem);

  // Expand variables whenever possible
  if (_is_cell(eexpr, c1) and opi::get_value(c1, eexpr))
    return _match_arguments(prt, ert, pexpr, eexpr, mem);
  if (_is_cell(pexpr, c1) and opi::get_value(c1, pexpr))
    return _match_arguments(prt, ert, pexpr, eexpr, mem);

  // Unify or assign variables
  if (_is_cell(eexpr, c1))
  {
    if (_is_cell(pexpr, c2))
      return opi::unify(c2, c1);
    else
      return not _is_tag(pexpr) and opi::unify(c1, prt.make_term(pexpr));
  }
  else if (_is_cell(pexpr, c1))
    return not _is_tag(eexpr) and opi::unify(c1, prt.make_term(eexpr));

  // Structural equality
  if (pexpr->t != eexpr->t)
    return false;

  switch (pexpr->t)
  {
    case opi::tag::pair:
      return _match_arguments(prt, ert, opi::car(pexpr), opi::car(eexpr), mem)
         and _match_arguments(prt, ert, opi::cdr(pexpr), opi::cdr(eexpr), mem);

    default:
      return opi::equal(pexpr, eexpr);
  }
}


bool
opi::match_arguments(opi::predicate_runtime &prt,
                     const opi::predicate_runtime &ert, opi::value pexpr,
                     opi::value eexpr)
{ return _match_arguments(prt, ert, pexpr, eexpr, opi::nil); }


static bool
_equivalent(opi::value x, opi::value y, opi::value expandmem, opi::value &equivmem)
{
  opi::value tmp = opi::nil;
  opi::cell *c1, *c2;

  // If asked for equivalence of (X, Y) to prove the same equivalence of (X, Y)
  // the answer is "yes"
  const opi::value theseargs = opi::cons(x, y);
  if (opi::member(theseargs, expandmem))
    return true;

  // Expand variables
  if (_is_cell(x, c1) and opi::get_value(c1, tmp))
    return _equivalent(tmp, y, opi::cons(theseargs, expandmem), equivmem);
  if (_is_cell(y, c1) and opi::get_value(c1, tmp))
    return _equivalent(x, tmp, opi::cons(theseargs, expandmem), equivmem);

  // Unbound variables:
  // 1) Assert that unbound variable is not equivalent to any (particular) value
  // 2) Identify not-yet-identified variables or confirm identity for identified
  if (_is_cell(x, c1))
  {
    if (_is_cell(y, c2))
    {
      c1 = opi::find(c1);
      c2 = opi::find(c2);
      // Variable is always identified to it-self
      if (c1 == c2)
        return true;
      // Assert identification if already present
      if (assoc(ptr(c1), equivmem, tmp))
        return tmp->ptr == c2;
      if (assoc(ptr(c2), equivmem, tmp))
        return tmp->ptr == c1;
      // Identify new pair of variables with each-other
      equivmem = cons(cons(ptr(c1), ptr(c2)), equivmem);
      equivmem = cons(cons(ptr(c2), ptr(c1)), equivmem);
      return true;
    }
    else
      return false;
  }
  else if (_is_cell(y, c1))
    return false;

  // Structural equality
  if (x->t != y->t)
    return false;
  if (x->t == opi::tag::pair)
  {
    return _equivalent(car(x), car(y), opi::cons(theseargs, expandmem), equivmem)
       and _equivalent(cdr(x), cdr(y), opi::cons(theseargs, expandmem), equivmem);
  }
  else
    return opi::equal(x, y);
}
  
bool
opi::equivalent(value x, value y)
{
  value expandmem = nil;
  value equivmem = nil;
  return _equivalent(x, y, expandmem, equivmem);
}


opi::assign_nonterminal_to::assign_nonterminal_to(value val): m_val {val} {}

void
opi::assign_nonterminal_to::operator () (predicate_runtime &rollbackprt,
                                         cell *x) const noexcept
{ unify(x, rollbackprt.make_term(m_val)); }

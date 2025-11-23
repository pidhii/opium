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
#include "opium/utilities/execution_timer.hpp"
#include "opium/logging.hpp"


static opi::object*
_initialize_cell(opi::object *ptr)
{
  static char sym[] = "#__Cell";
  ptr->_t = opi::tag::sym;
  ptr->_sym.data = sym;
  ptr->_sym.len = sizeof(sym);
  return ptr;
}

static opi::object cell_tag_object {opi::tag::sym};
const opi::value opi::cell_tag {_initialize_cell(&cell_tag_object)};


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
  const std::string_view xname = sym_name(x);
  const bool starts_with_capital = isupper((unsigned char)xname[0]);
  const bool starts_with_underscore = xname[0] == '_';
  is_wildcard = starts_with_underscore and xname.size() == 1;
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
  return opi::ispair(x) && opi::issym(opi::car(x), form_name);
}

static opi::value
_insert_cells(opi::predicate_runtime &prt, opi::value expr,
              opi::stl::unordered_map<opi::object *, opi::value> &mem,
              int quasiquote_level = 0)
{
  if (expr != "_")
  {
    const auto it = mem.find(&*expr);
    if (it != mem.end())
      return it->second;
  }

  if (opi::ispair(expr))
  {
    if (_is_quotation_form(expr, "quasiquote"))
    {
      // Validate expression
      assert(length(cdr(expr)) == 1);

      if (quasiquote_level == 0)
      {
        const opi::value result = _insert_cells(prt, car(cdr(expr)), mem, 1);
        mem.emplace(&*expr, result);
        // copy_location(expr, result); // NOTE don't know, might be needed
        return result;
      }
      else
      {
        opi::value result = cons("quasiquote", opi::nil);
        mem.emplace(&*expr, result);
        set_cdr(result, _insert_cells(prt, cdr(expr), mem, quasiquote_level + 1));
        copy_location(expr, result);
        return result;
      }
    }

    if (_is_quotation_form(expr, "unquote"))
    {
      // Validate expression
      assert(length(cdr(expr)) == 1);
      assert(quasiquote_level > 0);

      if (quasiquote_level == 1)
      { // Fully evaluate the argument
        const opi::value result = _insert_cells(prt, car(cdr(expr)), mem, 0);
        mem.emplace(&*expr, result);
        // copy_location(expr, result); // NOTE don't know, might be needed
        return result;
      }
      else
      { // Drop one quasiquote level but otherwize preserve the datum
        opi::value result = cons("unquote", opi::nil);
        mem.emplace(&*expr, result);
        set_cdr(result,
                _insert_cells(prt, cdr(expr), mem, quasiquote_level - 1));
        copy_location(expr, result);
        return result;
      }
    }

    if (_is_quotation_form(expr, "unquote-splicing"))
      throw opi::bad_code {std::format("Unimplemented syntax: {}", expr), expr};

    if (quasiquote_level == 0 and _is_quotation_form(expr, "quote"))
    {
      // Validate expression
      assert(length(cdr(expr)) == 1);

      const opi::value result = car(cdr(expr));
      // copy_location(expr, result); // NOTE don't know, might be needed
      return result;
    }
  }

  opi::value result = opi::nil;
  bool iswild;
  if (quasiquote_level == 0 and _is_variable(expr, iswild))
  {
    result = cons(opi::cell_tag, ptr(iswild ? opi::make_variable() : prt[expr]));
    mem.emplace(&*expr, result);
    copy_location(expr, result);
    return result;
  }
  else if (opi::ispair(expr))
  {
    result = cons(opi::nil, opi::nil);
    mem.emplace(&*expr, result);
    set_car(result, _insert_cells(prt, car(expr), mem, quasiquote_level));
    set_cdr(result, _insert_cells(prt, cdr(expr), mem, quasiquote_level));
    copy_location(expr, result);
    return result;
  }
  else
  {
    result = expr;
    mem.emplace(&*expr, result);
    return result;
  }
}

opi::value
opi::insert_cells(predicate_runtime &prt, value expr)
{
  OPI_FUNCTION_BENCHMARK
  opi::stl::unordered_map<object *, value> mem;
  return _insert_cells(prt, expr, mem, 0);
}


bool
opi::get_value(cell *x, value &result)
{
  OPI_FUNCTION_BENCHMARK
  x = find(x);
  if (x->kind == cell::kind::value)
  {
    result = x->val;
    return true;
  }
  return false;
}


opi::cell *
opi::find(cell *x)
{
  // No path optimization to preserve exact relational structure
  while (x->next != x)
    x = x->next;
  return x;
}


opi::cell *
opi::predicate_runtime::find(cell *x)
{
  // Path compression
  while (x->next != x)
  {
    m_history.emplace_back(x->next, x->next->next);
    x = x->next = x->next->next;
  }
  return x;
}


bool
opi::predicate_runtime::unify(cell *x, cell *y)
{
  cell* repx = this->find(x);
  cell* repy = this->find(y);

  // If they're already unified, nothing to do
  if (repx == repy)
    return true;

  // Case 1: Both are values
  if (repx->kind == cell::kind::value and repy->kind == cell::kind::value)
    throw std::runtime_error {"Can't unify two values"};

  // Case 2: x is a value, y is a variable
  if (repx->kind == cell::kind::value)
  {
    m_history.emplace_back(repy, repy->next);
    repy->next = repx;
    return true;
  }

  // Case 3: x is a variable, y is a value
  if (repy->kind == cell::kind::value)
  {
    m_history.emplace_back(repx, repx->next);
    repx->next = repy;
    return true;
  }

  // Case 4: Both are variables
  m_history.emplace_back(repx, repx->next);
  repx->next = repy;
  return true;
}


void
opi::predicate_runtime::unwind()
{
  for (const auto [c, n] : std::ranges::reverse_view(m_history))
    c->next = n;
  m_history.clear();
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
opi::predicate_runtime::variable(value var) const
{
  auto it = m_varmap.find(var);
  if (it == m_varmap.end())
    throw std::out_of_range {std::format("no such variable in PRT ({})", var)};
  else
    return it->second;
}



/**
 * Check if expression represents a cell (i.e. `(CELL . <pointer>)`) and
 * return the cell pointer if it is
 * 
 * \param expr Expression to check
 * \param result Output parameter to store the cell pointer
 * \return True if the expression represents a cell, false otherwise
 */
[[nodiscard]] static inline bool
_is_cell(opi::value expr, opi::cell *&result) noexcept
{
  if (opi::ispair(expr) and opi::is(opi::car(expr), opi::cell_tag))
  {
    result = static_cast<opi::cell*>(ptr_val(cdr(expr)));
    return true;
  }
  return false;
}


[[nodiscard]] static inline bool
_is_tag(opi::value x)
{ return opi::issym(x) and opi::sym_name(x)[0] == '#'; }


template <bool ForceMatch>
struct _match_arguments_impl {
  using mem_element_type = std::pair<opi::object*, opi::object*>;

  struct mem_element_hash {
    size_t
    operator () (const mem_element_type &x) const noexcept
    {
      size_t hash = 0;
      opi::hash_combine(hash, x.first);
      opi::hash_combine(hash, x.second);
      return hash;
    }
  };
  opi::stl::unordered_set<mem_element_type, mem_element_hash> mem;

  bool
  match(opi::predicate_runtime &prt, opi::value pexpr, opi::value eexpr)
  {
    opi::cell *c1, *c2;

    // Test if was already called with these arguments and return imediately if so
    OPI_BLOCK_BENCHMARK("memoisation things", {
      if (not mem.emplace(&*pexpr, &*eexpr).second)
        return true;
    })

    // Expand variables whenever possible
    OPI_BLOCK_BENCHMARK("variable expansion", {
      if (_is_cell(pexpr, c1))
        opi::get_value(prt.find(c1), pexpr);
      if (_is_cell(eexpr, c2))
        opi::get_value(prt.find(c2), eexpr);
    })

    // Unify or assign variables
    OPI_BLOCK_BENCHMARK("unification and assignment", {
      if (_is_cell(eexpr, c2))
      {
        if (_is_cell(pexpr, c1))
          return prt.unify(c1, c2) or ForceMatch;
        else
          return (not _is_tag(pexpr) and prt.unify(c2, opi::make_term(pexpr))) or ForceMatch;
      }
      else if (_is_cell(pexpr, c1))
        return (not _is_tag(eexpr) and prt.unify(c1, opi::make_term(eexpr))) or ForceMatch;
    })

    // Structural equality
    OPI_BLOCK_BENCHMARK("structural equality", {
      if (opi::tag(pexpr) != opi::tag(eexpr))
        return ForceMatch or false;

      switch (opi::tag(pexpr))
      {
        case opi::tag::pair:
          OPI_END_BENCHMARK();
          return match(prt, opi::car(pexpr), opi::car(eexpr)) and
                 match(prt, opi::cdr(pexpr), opi::cdr(eexpr));

        default:
          if constexpr (ForceMatch)
          {
            if (not opi::equal(pexpr, eexpr) and
                opi::loglevel >= opi::loglevel::debug)
            {
              opi::warning("Ignoring mismatch between lhs and rhs of "
                        "match_arguments():\nlhs = {}\nrhs = {}",
                        opi::reconstruct(pexpr, opi::stringify_unbound_variables),
                        opi::reconstruct(eexpr, opi::stringify_unbound_variables));
            }
            return true;
          }
          return opi::equal(pexpr, eexpr);
      }
    })
  }
}; // struct _match_arguments_impl


bool
opi::match_arguments(opi::predicate_runtime &prt, opi::value pexpr,
                     opi::value eexpr, bool force)
{
  OPI_FUNCTION_BENCHMARK
  if (force)
    return _match_arguments_impl<true>{}.match(prt, pexpr, eexpr);
  else
    return _match_arguments_impl<false>{}.match(prt, pexpr, eexpr);
}

static bool
_equivalent(opi::value x, opi::value y,
            opi::stl::unordered_set<opi::value> &argmem,
            opi::stl::unordered_map<void *, void *> &equivmem)
{
  opi::value tmp = opi::nil;
  opi::cell *c1, *c2;

  // If asked for equivalence of (X, Y) to prove the same equivalence of (X, Y)
  // the answer is "yes"
  const opi::value theseargs = opi::cons(x, y);
  if (argmem.contains(theseargs))
    return true;
  argmem.insert(theseargs);

  // Expand variables
  if (_is_cell(x, c1) and opi::get_value(c1, tmp))
    return _equivalent(tmp, y, argmem, equivmem);
  if (_is_cell(y, c1) and opi::get_value(c1, tmp))
    return _equivalent(x, tmp, argmem, equivmem);

  // Unbound variables:
  // 1) Assert that unbound variable is not equivalent to any (particular) value
  // 2) Identify not-yet-identified variables or confirm identity for identified
  if (_is_cell(x, c1))
  {
    if (_is_cell(y, c2))
    {
      c1 = opi::find(c1);
      c2 = opi::find(c2);
      // Variable is always equivalent to it-self
      if (c1 == c2)
        return true;

      // Assert identification if already present
      if (auto it = equivmem.find(c1); it != equivmem.end())
        return it->second == c2;
      if (auto it = equivmem.find(c2); it != equivmem.end())
        return it->second == c1;

      // Identify new pair of variables with each-other
      equivmem[c1] = c2;
      equivmem[c2] = c1;
      return true;
    }
    else
      return false;
  }
  else if (_is_cell(y, c1))
    return false;

  // Structural equality
  if (opi::tag(x) != opi::tag(y))
    return false;
  if (opi::ispair(x))
  {
    return _equivalent(car(x), car(y), argmem, equivmem)
       and _equivalent(cdr(x), cdr(y), argmem, equivmem);
  }
  else
    return opi::equal(x, y);
}
  
bool
opi::equivalent(value x, value y)
{
  OPI_FUNCTION_BENCHMARK
  opi::stl::unordered_map<void*, void*> equivmem;
  opi::stl::unordered_set<value> argmem;
  return _equivalent(x, y, argmem, equivmem);
}

bool
opi::equivalent_up_to_bindings(value x, value y)
{
  OPI_FUNCTION_BENCHMARK
  predicate_runtime tempns; // temporary frame for bindings
  return match_arguments(tempns, x, y);
}


opi::assign_nonterminal_to::assign_nonterminal_to(value val): m_val {val} {}

void
opi::assign_nonterminal_to::operator () (predicate_runtime &rollbackprt,
                                         cell *x) const noexcept
{ rollbackprt.unify(x, make_term(m_val)); }

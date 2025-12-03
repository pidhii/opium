#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/value.hpp"


namespace opi::detail {

// This has to be equivalent to result of composition of 
// signature -> reconstruct[stringify_unbound_variables]
//           -> _remove_dynamic_function_dispatch_body
//           -> insert_cells
// (unless i fucked it up)
//
// NOTE: Removal of dyn-dispatch bodies seems to be unnecessary (which is good);
//       however, it has significant (positive) impact on performance.
//
// TODO: I think this function can be made tail-callable (and so do many other
//       similar functions)
enum snapshot_mode { keep_body, remove_body };

template <snapshot_mode Mode>
static value
snapshot(value s, stl::unordered_map<const object *, value> &argmem,
         stl::unordered_map<const cell *, value> &cellmem)
{
  const auto it = argmem.find(&*s);
  if (it != argmem.end())
    return it->second;

  if (ispair(s))
  {
    if (is(car(s), cell_tag))
    {
      cell *repr = find(ptr_val<cell*>(cdr(s)));
      value val;
      if (get_value(repr, val))
        return argmem[&*s] = snapshot<Mode>(val, argmem, cellmem);
      else
      {
        const auto it = cellmem.find(repr);
        if (it != cellmem.end())
          return it->second;
        else
          return cellmem[repr] = make_cell(make<cell>());
      }
    }
    else if (Mode == snapshot_mode::remove_body and
             car(s) == "#dynamic-function-dispatch")
    {
      return argmem[&*s] = list(snapshot<Mode>(list_ref(s, 0), argmem, cellmem),  // tag
                                snapshot<Mode>(list_ref(s, 1), argmem, cellmem),  // name
                                snapshot<Mode>(list_ref(s, 2), argmem, cellmem),  // args
                                snapshot<Mode>(list_ref(s, 3), argmem, cellmem)); // results
    }
    else
    {
      value &result = argmem.emplace(&*s, cons(nil, nil)).first->second;
      set_car(result, snapshot<Mode>(car(s), argmem, cellmem));
      set_cdr(result, snapshot<Mode>(cdr(s), argmem, cellmem));
      return result;
    }
  }
  else
    return argmem[&*s] = s;
}

template <snapshot_mode Mode>
static value
snapshot(value signature)
{
  OPI_FUNCTION_BENCHMARK
  stl::unordered_map<const object *, value> argmem;
  stl::unordered_map<const cell *, value> cellmem;
  return snapshot<Mode>(signature, argmem, cellmem);
}


static value
remove_bodies(value s, stl::unordered_map<const object *, value> &argmem)
{
  const auto it = argmem.find(&*s);
  if (it != argmem.end())
    return it->second;

  if (ispair(s))
  {
    if (is(car(s), cell_tag))
    {
      cell *repr = find(ptr_val<cell*>(cdr(s)));
      value val;
      if (get_value(repr, val))
        return argmem[&*s] = remove_bodies(val, argmem);
      else
        return argmem[&*s] = s;
    }
    else if (car(s) == "#dynamic-function-dispatch")
    {
      return argmem[&*s] = list(remove_bodies(list_ref(s, 0), argmem),  // tag
                                remove_bodies(list_ref(s, 1), argmem),  // name
                                remove_bodies(list_ref(s, 2), argmem),  // args
                                remove_bodies(list_ref(s, 3), argmem)); // results
    }
    else
    {
      value &result = argmem.emplace(&*s, cons(nil, nil)).first->second;
      set_car(result, remove_bodies(car(s), argmem));
      set_cdr(result, remove_bodies(cdr(s), argmem));
      return result;
    }
  }
  else
    return argmem[&*s] = s;
}

inline value
remove_bodies(value s)
{
  OPI_FUNCTION_BENCHMARK
  stl::unordered_map<const object *, value> mem;
  return remove_bodies(s, mem);
}


template <snapshot_mode Mode = snapshot_mode::keep_body>
struct effects_memoization {
  bool
  same_effect(value effect) const noexcept
  {
    if constexpr (Mode == remove_body)
      effect = remove_bodies(effect);
    for (value memeffect : range(m_effects_memory))
    {
      if (equivalent(effect, memeffect))
        return true;
    }
    return false;
  }

  void
  save_effect(value effect) noexcept
  {
    const value effectsave = snapshot<Mode>(effect);
    m_effects_memory = cons(effectsave, m_effects_memory);
  }

  private:
  value m_effects_memory;
}; // struct opi::detail::effects_memoization

} // namespace opi::detail
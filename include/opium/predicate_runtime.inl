/**
 * \file predicate_runtime.ini
 * Template implementation of opium/predicate_runtime.hpp members 
 */
#pragma once

#include "opium/predicate_runtime.hpp"

#include <cassert>


namespace opi::detail {

/**
 * Helper struct for reconstructing values from cells
 * 
 * \tparam UVHandler Type of unbound variable handler
 */
template <unbound_variable_handler UVHandler>
struct _reconstructor {
  UVHandler uvhandler; /**< Handler for unbound variables */
  opi::stl::unordered_map<cell *, value> mem; /**< Memoization map to avoid infinite recursion */
  opi::stl::unordered_map<value, value> pairmem;

  /**
   * Constructor
   * 
   * \tparam T Type of unbound variable handler
   * \param uvhandler Handler for unbound variables
   */
  template <typename T>
  _reconstructor(T &&uvhandler): uvhandler {std::forward<T>(uvhandler)} { }

  /**
   * Reconstruct a value
   * 
   * \param x Value to reconstruct
   * \return Reconstructed value
   */
  opi::value
  _reconstruct(opi::value x)
  {
    if (x->t == opi::tag::pair)
    {
      if (opi::issym(car(x), CELL))
        return _reconstruct(static_cast<opi::cell *>(x->cdr->ptr));
      else
      {
        // Avoid infinite recursion
        const auto it = pairmem.find(x);
        if (it != pairmem.end())
          return it->second;
        // Create and memorize placeholder-pair to be filled in later
        value result = cons(nil, nil);
        pairmem.emplace(x, result);
        result->car = &*_reconstruct(car(x));
        result->cdr = &*_reconstruct(cdr(x));
        return result;
      }
    }
    else
      return x;
  }

  /**
   * Reconstruct a value from a cell
   * 
   * \param x Cell to reconstruct from
   * \return Reconstructed value
   */
  opi::value
  _reconstruct(opi::cell *x)
  {
    x = find(x);

    const auto it = mem.find(x);
    if (it != mem.end())
      return it->second;

    switch (x->kind)
    {
      case opi::cell::kind::value:
        if (x->val->t == opi::tag::pair)
        {
          const opi::value val = opi::cons(opi::nil, opi::nil);
          mem.emplace(x, val);
          const value newcar = _reconstruct(car(x->val));
          const value newcdr = _reconstruct(cdr(x->val));
          assert(&*newcar);
          assert(&*newcdr);
          val->car = &*newcar;
          val->cdr = &*newcdr;
          return val;
        }
        else
          return x->val;

      case opi::cell::kind::variable:
        return uvhandler(x);
    }
    std::terminate();
  }

}; // struct opi::detail::_reconstructor

} // namespace opi::detail


template <opi::unbound_variable_handler UVHandle>
opi::value
opi::reconstruct(cell *x, UVHandle uvhandler)
{
  return opi::detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}


template <opi::unbound_variable_handler UVHandle>
opi::value
opi::reconstruct(value x, UVHandle uvhandler)
{
  return detail::_reconstructor<std::remove_cvref_t<UVHandle>> {uvhandler}
      ._reconstruct(x);
}

template <opi::nonterminal_variable_handler NTVHandler>
bool
opi::predicate_runtime::try_sign(const void *preduid, value signature,
                                 const predicate_runtime &prev,
                                 NTVHandler ntvhandler) noexcept
{
  for (const predicate_runtime *prt = &prev; prt; prt = prt->m_prev_frame)
  {
    if (preduid == prt->m_preduid)
    {
      assert(prt->m_preduid != nullptr);
      if (equivalent(signature, prt->m_signature))
      {
        // Do the actual match as we may be dealing with recursive types instead
        // if just non-terminals
        const bool ok = match_arguments(*this, *prt, signature, prt->m_signature);
        assert(ok and "Failed to match equivalent signatures");

        // Process non-terminal variables
        if constexpr (not std::is_same_v<NTVHandler, ignore_nonterminal_variables>)
        {
          for (const value var : variables())
          {
            // Variables present in signature but not bound by `match_arguments`
            // are regarded as non-terminal (computation of their type will not
            // terminate).
            // Use `reconstruct` to scan for (possibly) nested unbound variables
            // variables and trigger user-handler (`ntvhandler`) on each of them.
            reconstruct((*this)[var], [&](cell *x) {
              ntvhandler(*this, x);
              return nil;
            });
          }
        }
        return false; // Notify about signature clash
      }
    }
  }

  // No chash encountered. Add this runtime into the chain and report success
  m_preduid = preduid;
  m_signature = signature;
  m_prev_frame = &prev;
  return true;
}

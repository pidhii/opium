// Template implementation of opium/predicate_runtime.hpp members 
#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/logging.hpp"

#include <cassert>
#include <functional>


namespace opi::detail {

template <unbound_variable_handler UVHandler>
struct _reconstructor {
  UVHandler uvhandler;
  opi::unordered_map<cell *, value> mem;

  template <typename T>
  _reconstructor(T &&uvhandler): uvhandler {std::forward<T>(uvhandler)} { }

  opi::value
  _reconstruct(opi::value x)
  {
    if (x->t == opi::tag::pair)
    {
      if (opi::issym(car(x), "__cell"))
        return _reconstruct(static_cast<opi::cell *>(x->cdr->ptr));
      else
        return cons(_reconstruct(car(x)), _reconstruct(cdr(x)));
    }
    else
      return x;
  }

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
          val->car = &*_reconstruct(car(x->val));
          val->cdr = &*_reconstruct(cdr(x->val));
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
      const bool issimilar =
          match_arguments(*this, *prt, signature, prt->m_signature);
      debug("compare {} vs {} -> {}", signature, prt->m_signature, issimilar);
      if (issimilar)
      {
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

      // Clean up this runtime and proceed with clash-scann
      mark_dead();
      m_varmap.clear();
    }
  }

  // No chash encountered. Add this runtime into the chain and report success
  m_preduid = preduid;
  m_signature = signature;
  m_prev_frame = &prev;
  return true;
}


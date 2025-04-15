#pragma once

#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <iterator>


namespace opi {


using query_result =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;


struct scheme_emitter {
  scheme_emitter(scheme_emitter_context &ctx, query_result &query);

  template <std::output_iterator<value> ExprOutput>
  void
  emit(value expr, ExprOutput exproutput)
  {
    const value result = m_transformer(expr);
    if (not is(result, m_dont_emit_symbol))
      *exproutput++ = result;
  }

  protected:
  value
  _find_code_type(value code) const;

  private:
  const value m_dont_emit_symbol;
  const query_result &m_query_result;
  scheme_code_transformer m_transformer;
  scheme_emitter_context &m_ctx;
}; // class opi::scheme_emitter


} // namespace opi

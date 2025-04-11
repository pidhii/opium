#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/source_location.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/pretty_print.hpp"
#include "opium/exceptions.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <iterator>
#include <ranges>


namespace opi {


using query_result =
    opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;


template <std::output_iterator<value> Output>
struct scheme_emitter {
  scheme_emitter(scheme_emitter_context<Output> &ctx, query_result &query);

  template <std::output_iterator<value> ExprOutput>
  void
  emit(value expr, ExprOutput exproutput);

  protected:
  value
  _find_code_type(value code) const;

  private:
  const value m_dont_emit_symbol;
  const query_result &m_query_result;
  scheme_code_transformer m_transformer;
  scheme_emitter_context<Output> &m_ctx;
}; // class opi::scheme_emitter


template <std::output_iterator<value> Output>
value
_emit_specialized_function_body(scheme_emitter_context<Output> &ctx,
                                const predicate &pred, value ppbody,
                                value instantiation);


template <std::output_iterator<value> Output>
value
_instantiate_function_template(scheme_emitter_context<Output> &ctx,
  value instantiation, const predicate &pred, value ppcode);


template <std::output_iterator<value> Output>
value
instantiate(scheme_emitter_context<Output> &ctx, value type, value x);


template <std::output_iterator<value> Output>
std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context<Output> &ctx, value plcode, value ppcode);


void
pretty_template_instance_name(value type, std::ostream &os);


inline std::pair<value, scheme_type_location_map>
translate_to_scheme(size_t &counter, prolog &pl, value code)
{
  // Compose translator from Scheme to Prolog
  symbol_generator genuid {counter, "uid{}"};
  scheme_code_flattener flatten {genuid};
  scheme_unique_identifiers insert_uids {genuid};
  scheme_to_prolog to_prolog {counter};
  prolog_cleaner pl_cleaner;

  // TODO: find a better way
  int warned = false;
  opi::stl::vector<value> extforwardtypes;
  for (const auto &[predicatename, predicate] : pl.predicates())
  {
    if (predicatename == "result-of")
    {
      const value signature = predicate.argument(0);
      if (signature->t == tag::pair and issym(car(signature)))
      {
        const value identifier = car(signature);

        if (not warned++)
          warning("extracting forward-types from support predicates");
        to_prolog.add_global(identifier, identifier);
        extforwardtypes.push_back(identifier);
      }
    }
  }

  // Translate the input expression
  const value ppcode = list(range(code)
                     | std::views::transform(std::ref(flatten))
                     | std::views::transform(std::ref(insert_uids)));
  const value plcode = list(range(to_prolog.transform_block(ppcode)) |
                            std::views::transform(std::ref(pl_cleaner)));

  debug("type-check Prolog code:\n{}", pprint_pl(plcode));

  const value unresolved = list(to_prolog.unresolved());
  if (length(unresolved) > 0)
  {
    for (const value symbol : range(unresolved))
    {
      error("unresolved symbol: \e[1m‘{}’\e[0m", symbol);
      source_location location;
      if (get_location(symbol, location))
        std::cerr << display_location(location, 0, "\e[38;5;1;1m") << std::endl;
    }
    throw bad_code {"unresolved symbols", unresolved};
  }

  // Collect predicates generated during translation
  for (const predicate &pred : to_prolog.predicates())
  {
    if (loglevel >= loglevel::info)
    {
      debug("add generated predicate:\n{}{} :-\n  {}", pred.name(),
            list(pred.arguments()), pprint_pl(pl_cleaner(pred.body()), 2));
    }
    pl.add_predicate(pred);
  }

  // Translate the code to proper scheme
  std::deque<value> main_tape;
  scheme_emitter_context ctx {pl, to_prolog, std::back_inserter(main_tape)};
  ctx.legal_types.insert(extforwardtypes.begin(), extforwardtypes.end());
  auto [main, type_map] = emit_scheme(ctx, plcode, ppcode);

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {resultcode, type_map};
}


} // namespace opi

#include "opium/scheme/scheme_emitter.inl" // IWYU pragma: export
#include "opium/scheme/scheme_type_system.inl" // IWYU pragma: export

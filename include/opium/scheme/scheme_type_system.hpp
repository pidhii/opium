#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/pretty_print.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_emitter.hpp"
#include "opium/value.hpp"

#include <ranges>


namespace opi {



std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode);


void
pretty_template_instance_name(value type, std::ostream &os);


value
instantiate(scheme_emitter_context &ctx, value type, value x);


value
_instantiate_function_template(scheme_emitter_context &ctx, value instantiation,
                               value typetemplate, value ppdefinition,
                               scheme_emitter_context &template_ctx);


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
  debug("\e[1mPre-Processor output:\e[0m");
  for (const value x : range(ppcode))
    debug("{}\n\n", pprint_scm(x));

  const value plcode = list(range(to_prolog.transform_block(ppcode)) |
                            std::views::transform(std::ref(pl_cleaner)));

  // Collect predicates generated during translation
  debug("\e[1mType Check predicates:\e[0m");
  for (const predicate &pred : to_prolog.predicates())
  {
    if (loglevel >= loglevel::debug)
    {
      debug("```\n(predicate {}\n  {}\n```",
            cons(sym(pred.name()), list(pred.arguments())),
            pprint_pl(pl_cleaner(pred.body()), 2));
    }
    pl.add_predicate(pred);
  }

  debug("\e[1mType Check Prolog code:\e[0m\n```\n{}```", pprint_pl(plcode));

  // Translate the code to proper scheme
  opi::stl::vector<value> main_tape;
  scheme_emitter_context ctx {pl, to_prolog, main_tape};
  ctx.legal_types.insert(extforwardtypes.begin(), extforwardtypes.end());
  auto [main, type_map] = emit_scheme(ctx, plcode, ppcode);

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {resultcode, type_map};
}

} // namespace opi

#include "opium/scheme/scheme_emitter.inl" // IWYU pragma: export
#include "opium/scheme/scheme_type_system.inl" // IWYU pragma: export

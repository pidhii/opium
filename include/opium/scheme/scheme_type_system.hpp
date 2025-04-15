#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/prolog.hpp"
#include "opium/pretty_print.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <ranges>


namespace opi {



std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode);


value
instantiate_function_template(scheme_emitter_context &ctx, value type);


/**
 * Generate a distinctive name for a template instance using parameter types for
 * unique identification
 *
 * This function creates a string representation of a template instance in the format
 * `identifier<param1,param2,...>` with recursive handling of parameter types.
 * 
 * \param type The template instance type value
 * \param os The output stream to write the formatted name to
 */
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
  auto [main, type_map] = emit_scheme(ctx, plcode, ppcode);

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {resultcode, type_map};
}

} // namespace opi

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


#pragma once

#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/prolog.hpp"
#include "opium/pretty_print.hpp"
#include "opium/utilities/execution_timer.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <fstream>
#include <iterator>
#include <ranges>


namespace opi {


std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode);


/**
 * Generate implementation of a function template specialization
 *
 * \param ctx Current emitter-context
 * \param instantiation Function template instantiation from the TypeChecker
 * \param typetemplate Function template from the TypeChecker
 * \param ppbody Actual function body form the (preprocessed) input code
 * \param template_ctx Native context of the function template
 */
value
generate_function_template_body(scheme_emitter_context &ctx,
                                value instantiation, value type_template,
                                value ppbody);


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


template <std::output_iterator<value> Output>
void
_gather_literals(value x, Output output) noexcept
{
  if (issym(x) and x != "_" and std::islower(sym_name(x)[0]))
    *output++ = x;
  else if (x->t == tag::pair)
  {
    _gather_literals(car(x), output);
    _gather_literals(cdr(x), output);
  }
}

template <std::ranges::range Pragmas = std::initializer_list<value>>
inline std::pair<value, scheme_type_location_map>
translate_to_scheme(size_t &counter, prolog &pl, value ppcode,
                    Pragmas &&pragmas = {})
{
  // Compose translator from Scheme to Prolog
  scheme_to_prolog to_prolog {counter};
  prolog_cleaner pl_cleaner;

  // Configure Prolog interpreter
  to_prolog.set_up_prolog(pl);

  // TODO: find a better way
  int warned = false;
  for (const auto &[predicatename, predicate] : pl.predicates())
  {
    if (predicatename == "result-of")
    {
      const value signature = car(cdr(predicate.signature()));
      if (signature->t == tag::pair and issym(car(signature)))
      {
        const value identifier = car(signature);

        if (not warned++)
          warning("extracting types from support predicates");
        debug("add global: {}", identifier);
        to_prolog.add_global(identifier, identifier);
      }
    }
  }

  execution_timer prolog_generation_timer {"Prolog generation"};
  const value plcode = list(range(to_prolog.transform_block(ppcode))
                            | std::views::transform(std::ref(pl_cleaner)));
  prolog_generation_timer.stop();

  if (global_flags.contains("DumpTypeCheck"))
  {
    if (std::ofstream file {"TypeCheck.scm"})
      file << pprint_pl(plcode) << std::endl;
    else
      warning("Failed to open TypeCheck.scm for writing");
  }

  debug("\e[1mType Check Prolog code:\e[0m\n```\n{}\n```", pprint_pl(plcode));

  // Translate the code to proper scheme
  opi::stl::vector<value> main_tape;
  scheme_emitter_context ctx {pl, to_prolog, main_tape};

  // Process pragmas
  const match matchcasesrule {list("cases-rule"),
                         list("cases-rule", "ctor-pattern", "type-pattern",
                              "predicate", "unpack")};
  const match matchinline {list("inline"), cons("inline", "exprs")};
  opi::stl::unordered_map<value, value> ms;
  opi::stl::vector<value> inlines;
  for (const value expr : pragmas)
  {
    if (ms.clear(), matchcasesrule(expr, ms))
    {
      const value ctorpattern = ms.at("ctor-pattern");
      const value typepattern = ms.at("type-pattern");
      const value predicate = ms.at("predicate");
      const value unpack = ms.at("unpack");

      std::vector<value> ctorliterals, typeliterals;
      _gather_literals(ctorpattern, std::back_inserter(ctorliterals));
      _gather_literals(typepattern, std::back_inserter(typeliterals));

      ctx.add_case_rule({
        match {list(ctorliterals), ctorpattern},
        match {list(typeliterals), typepattern},
        predicate,
        unpack,
      });
    }
    else if (ms.clear(), matchinline(expr, ms))
    {
      const value exprs = ms.at("exprs");
      std::ranges::copy(range(exprs), std::back_inserter(inlines));
    }
    else
      throw bad_code {std::format("Invalid pragma: {}", expr), expr};
  }

  execution_timer emit_timer {"Scheme emitter"};
  auto [main, type_map] = emit_scheme(ctx, plcode, ppcode);
  emit_timer.stop();

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {append(list(inlines), resultcode), type_map};
}

} // namespace opi

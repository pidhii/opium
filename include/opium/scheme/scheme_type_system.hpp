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
#include "opium/prolog_repl.hpp"
#include "opium/pretty_print.hpp"
#include "opium/utilities/execution_timer.hpp"

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/value.hpp"

#include <exception>
#include <fstream>
#include <iterator>
#include <ranges>


namespace opi {


struct typecheck_failure: std::exception {
  typecheck_failure(const std::string &what, scheme_type_location_map &&tlm)
  : _what {what}, tlm {std::move(tlm)}
  { }

  const char*
  what() const noexcept override
  { return _what.c_str(); }

  std::string _what;
  scheme_type_location_map tlm;
};


struct interupt_emit: std::exception {
  interupt_emit(std::string_view reason): reason {reason} { }

  const char *
  what() const noexcept
  { return reason.c_str(); }

  std::string reason;
};


std::pair<value, scheme_type_location_map>
emit_scheme(scheme_emitter_context &ctx, value plcode, value ppcode,
            const std::optional<prolog_guide_function> &guide);


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
  if (issym(x) and x != "_" and not std::isupper(sym_name(x)[0]))
    *output++ = x;
  else if (ispair(x))
  {
    _gather_literals(car(x), output);
    _gather_literals(cdr(x), output);
  }
}


struct default_type_coder {
  value
  operator () (value x) const
  {
    switch (tag(x))
    {
      case tag::nil: return "nil";
      case tag::num: return "num";
      case tag::str: return "str";
      case tag::boolean: return "bool";
      default: throw bad_code {"default_typ_coder - unsupported type", x};
    }
  }
};


struct scheme_translator {
  scheme_translator()
  : type_coder {default_type_coder()}
  { prolog_emitter::setup_prolog(prolog); }

  opium_preprocessor preprocessor;
  opi::prolog_repl prolog;
  prolog_emitter::type_coder type_coder;
  mutable size_t counter;
};


template <std::ranges::range Pragmas = std::initializer_list<value>>
static std::pair<value, scheme_type_location_map>
translate_to_scheme(
    const scheme_translator &translator_config, value ppcode,
    Pragmas &&pragmas = {},
    const std::optional<prolog_guide_function> &guide = std::nullopt)
{
  // Utility variable
  opi::stl::unordered_map<value, value> ms;

  // Compose translator from Scheme to Prolog
  code_type_map code_types;
  prolog_emitter to_prolog {translator_config.counter,
                            translator_config.type_coder, code_types};
  prolog_cleaner pl_cleaner;

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
  scheme_emitter_context ctx {translator_config.prolog, code_types, main_tape};

  // Process pragmas
  const match matchcasesrule {list("cases-rule"),
                         list("cases-rule", "ctor-pattern", "type-pattern",
                              "predicate", "unpack")};
  const match matchinline {list("inline"), cons("inline", "exprs")};
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
  }

  execution_timer emit_timer {"Scheme emitter"};
  auto [main, type_map] = emit_scheme(ctx, plcode, ppcode, guide);
  emit_timer.stop();

  const value globals = list(main_tape);
  const value resultcode = append(globals, main);
  return {append(list(inlines), resultcode), type_map};
}

} // namespace opi

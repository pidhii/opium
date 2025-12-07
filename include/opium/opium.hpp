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

#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/prolog_repl.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/scheme/translator/match_translation_rules.hpp"
#include "opium/value.hpp"


namespace opi {

// Translation steps:
// 1. opicode -> [PP] -> ircode
// 2. ircode -> [Prolog emitter] -> plcode + code_types
// 3. plcode -> [Prolog] -> type_bindings
//
// Target-specific:
// 4. ircode + code_types + type_bindings -> [target language emitter] -> tgtcode
//
// By-products:
// - ircode + code_types -> raw_tlm
//   Useful for tracing debugging, when typecheck fails, and so true_tlm is
//   unaccessible.
// - raw_tlm(=ircode+code_types) + type_bindings -> true_tlm
//   Fully annotated code (besides natural ambiguities in templates bodies).

struct program {
  using type_bindings_t =
      opi::stl::unordered_map<value, opi::stl::unordered_set<value>>;

  std::optional<value> ircode;
  std::optional<value> typecheck_script;
  std::optional<code_type_map> code_types;
  std::optional<type_bindings_t> type_bindings;
  std::optional<value> typecheck_save;
};

struct scheme_program: public program {
  std::optional<value> scheme_script;
};

// struct translator_state {
//   opium_preprocessor &preprocessor;
//   prolog_emitter &typer;
//   value &typecheck_save;
// };


void
generate_ir(program &program, opium_preprocessor &pp, value opicode);

void
generate_typecheck_script(program &program, prolog_emitter &typer);

bool
typecheck(program &program, const prolog &prolog,
          const prolog_guide_function &guide = nullptr);

bool
incremental_typecheck(program &program, const prolog &prolog,
                      predicate_runtime &typecheckns,
                      const prolog_guide_function &guide = nullptr);

bool
generate_scheme(scheme_program &scmprogram,
                const match_translation_rules &match_translation,
                const scheme_emitter_context::literal_coder &literal_coder);


struct default_literal_coder {
  value
  operator () (value literal, value type) const
  {
    switch (tag(literal))
    {
      case tag::num:
        assert(type == "num");
        return literal;

      case tag::str:
        assert(type == "str");
        return literal;

      case tag::boolean:
        assert(type == "bool");
        return literal;

      default:
        throw bad_code {
            std::format("default_literal_coder - unsupported literal ({})",
                        literal),
            literal};
    }
  }
};


struct scheme_translator {
  scheme_translator()
  : literal_coder {default_literal_coder()}
  { prolog_emitter::setup_prolog(prolog); }

  opium_preprocessor preprocessor;
  prolog_emitter typer;
  prolog_repl prolog;
  scheme_emitter_context::literal_coder literal_coder;
  match_translation_rules match_translation;
  value prologue;

  mutable size_t counter;
};

void
translate_to_scheme(scheme_translator &config, value opicode,
                    scheme_program &scmprogram,
                    const prolog_guide_function &guide = nullptr);

} // namespace opi
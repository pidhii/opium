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

#include "opium/opium.hpp"
#include "opium/logging.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/translator/match_translation_rules.hpp"
#include "opium/scheme/translator/scheme_emitter.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/scheme/translator/exceptions.hpp"


template <typename T>
static inline T &
_access(std::optional<T> &opt, std::string_view fail_message)
{
  if (not opt.has_value())
    throw std::runtime_error {std::string {fail_message}};
  return opt.value();
}

void
opi::generate_ir(program &program, opium_preprocessor &pp, value opicode)
{
  OPI_FUNCTION_BENCHMARK

  // Run preprocessor
  info("\e[1mrunning preprocessor\e[0m");
  program.ircode = pp.transform_block(opicode);
}


void
opi::generate_typecheck_script(program &program, prolog_emitter &typer)
{
  OPI_FUNCTION_BENCHMARK

  // Emit TypeCheck script
  info("\e[1mgenerating TypeCheck script\e[0m");
  const value ircode = _access(program.ircode, "Can't generate TypeCheck script"
                                               ", program is missing IR-code");
  code_type_map &ctm = program.code_types.emplace();
  program.typecheck_script = clean_prolog(typer.transform_block(ircode, ctm));
}


bool
opi::incremental_typecheck(program &program, const prolog &prolog,
                           predicate_runtime &typecheckns,
                           const prolog_guide_function &guide)
{
  OPI_FUNCTION_BENCHMARK

  const value plcode = _access(program.typecheck_script,
                               "Can't run TypeCheck, program is missing"
                               " TypeCheck script");
  type_bindings &results = program.type_bindings.emplace();
  const value ccode = insert_cells(typecheckns, plcode);

  // Function to save query results
  int successes = 0;
  auto save_results = [&]() {
    if (++successes > 1)
      return;

    // Record all variables
    for (const value varname : typecheckns.variables())
    {
      execution_timer timer {"reconstruct query results"};
      const value cval =
          reconstruct<clone_unbound_variables_t>(typecheckns[varname]);
      timer.stop();
      results[varname].insert(cval);
    }

    // Save typecheck state
    execution_timer _ {"save typecheck state"};
    program.typecheck_save = snapshot(ccode);
  };

  // Run the query
  // disable implicit location propagation for better performance
  opi::utl::state_saver _ {g_propagate_locations_on_cons};
  g_propagate_locations_on_cons = false;
  info("\e[1mrunning TypeCheck\e[0m");
  if (guide)
    prolog.make_true(ccode, save_results, guide);
  else
    prolog.make_true(ccode, save_results);

  if (successes > 1)
    error("Ambiguous typecheck, {} possible interpretations were found",
            successes);

  return successes == 1;
}


bool
opi::typecheck(program &program, const prolog &prolog,
               const prolog_guide_function &guide)
{
  predicate_runtime prt;
  return incremental_typecheck(program, prolog, prt, guide);
}


bool
opi::generate_scheme(scheme_program &scmprogram,
                     const match_translation_rules &mtrs,
                     const scheme_emitter_context::literal_coder &lc)
{
  OPI_FUNCTION_BENCHMARK

  static const char no_ir_msg[] =
      "Can't generate Scheme, program is missing IR code";
  static const char no_ctm_msg[] =
      "Can't generate Scheme, program is missing code-to-type associatoins";
  static const char no_tbinds_msg[] =
      "Can't generate Scheme, program is missing type bindings";

  stl::vector<value> headertape, maintape;
  const value ircode = _access(scmprogram.ircode, no_ir_msg);
  const code_type_map &ctm = _access(scmprogram.code_types, no_ctm_msg);
  const type_bindings &tbinds = _access(scmprogram.type_bindings, no_tbinds_msg);

  scheme_emitter_context ctx {ctm, mtrs, lc, headertape};

  // Translate `program` into Scheme
  info("\e[1mgenerating Scheme\e[0m");
  scheme_emitter emitter {ctx, tbinds};
  for (const value expr : range(ircode))
    emitter.emit(expr, std::back_inserter(maintape));

  value &scmcode = scmprogram.scheme_script.emplace(nil);
  append_mut(scmcode, list(headertape));
  append_mut(scmcode, list(maintape));

  return true;
}


void
opi::translate_to_scheme(scheme_translator &config, value opicode,
                         scheme_program &scmprogram,
                         const prolog_guide_function &guide)
{
  OPI_FUNCTION_BENCHMARK

  generate_ir(scmprogram, config.preprocessor, opicode);
  generate_typecheck_script(scmprogram, config.typer);
  if (not typecheck(scmprogram, config.prolog, guide))
    throw typecheck_failure {"TypeCheck failed"};
  if (not generate_scheme(scmprogram, config.match_translation,
                          config.literal_coder))
    throw typecheck_failure {"Scheme generation failed"};
  scmprogram.scheme_script = append(config.prologue, *scmprogram.scheme_script);
}

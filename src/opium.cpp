#include "opium/opium.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include <optional>


using pragmas =
    opi::stl::unordered_map<std::string, opi::stl::deque<opi::value>>;


void
_find_pragmas(opi::value script, pragmas &pragmas)
{
  opi::stl::unordered_map<opi::value, opi::value> matches;
  for (const opi::value expr : range(script))
  {
    // Handle pragmas
    if (opi::ispair(expr) and car(expr) == "pragma")
    {
      if (length(cdr(expr)) < 2 or not issym(car(cdr(expr))))
        throw opi::bad_code {"Invalid pragma", expr};

      const std::string tag {sym_name(car(cdr(expr)))};
      std::ranges::copy(range(cdr(cdr(expr))), std::back_inserter(pragmas[tag]));
    }
  }
}


static void
_write_scheme_script(std::ostream &os, opi::value script)
{
  os << "(cond-expand (guile (import (srfi :11))) (else))\n\n";
  for (const opi::value expr : range(script))
    os << opi::strip_escape_sequences(pprint_scm(expr)) << "\n\n";
}


// FIXME: it has to be somehow a part of preprocessor
void
opi::apply_prolog_pragmas(opi::value opiprogram, opi::prolog_repl &pl)
{
  // Collect and erase pragmas
  pragmas pragmas;
  _find_pragmas(opiprogram, pragmas);

  // Run extra prolog expressions
  for (const value plexpr : pragmas["prolog"])
    pl << plexpr;
}


void
opi::generate_scheme(opi::value in, opi::scheme_preprocessor &pp,
                     const opi::prolog &pl, const std::filesystem::path &opath,
                     const std::optional<prolog_guide_function> &guide)
{
  using namespace opi;

  // Collect and erase pragmas
  pragmas pragmas;
  _find_pragmas(in, pragmas);

  opi::execution_timer preprocessor_timer {"Preprocessor"};
  const value ppcode = pp.transform_block(in);
  preprocessor_timer.stop();

  info("\e[1mrunning Type Check\e[0m");
  opi::execution_timer analyzer_timer {"Type analyzer"};
  size_t cnt = 0;
  const auto [out, tlm] =
      translate_to_scheme(cnt, pl, ppcode, pragmas["scheme-translator"], guide);
  analyzer_timer.stop();

  // Write generated Scheme script
  if (not opath.empty())
  {
    info("writing Scheme script to {}", opath.c_str());
    if (std::ofstream ofile {opath})
      _write_scheme_script(ofile, out);
    else
      throw std::runtime_error {
          std::format("Failed to open file {} for writing", opath.c_str())};
  }
}

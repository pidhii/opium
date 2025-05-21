#include "opium/opium.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_type_system.hpp"


using pragmas =
    opi::stl::unordered_map<std::string, opi::stl::deque<opi::value>>;


opi::value
_filter_pragmas(opi::value script, pragmas &pragmas)
{
  opi::stl::vector<opi::value> result;
  opi::stl::unordered_map<opi::value, opi::value> matches;
  for (const opi::value expr : range(script))
  {
    // Handle pragmas
    if (expr->t == opi::tag::pair and car(expr) == "pragma")
    {
      if (length(cdr(expr)) < 2 or not issym(car(cdr(expr))))
        throw opi::bad_code {"Invalid pragma", expr};

      const std::string tag {sym_name(car(cdr(expr)))};
      std::ranges::copy(range(cdr(cdr(expr))), std::back_inserter(pragmas[tag]));
    }
    else
    {
      // Pass all other expressions
      result.push_back(expr);
    }
  }

  return list(result);
}


static void
_write_scheme_script(std::ostream &os, opi::value script)
{
  os << "(cond-expand (guile (import (srfi :11))) (else))\n\n";
  for (const opi::value expr : range(script))
    os << opi::strip_escape_sequences(pprint_scm(expr)) << "\n\n";
}


void
opi::generate_scheme(opi::value in, opi::prolog_repl &pl,
                     const std::filesystem::path &opath)
{
  using namespace opi;

  // Collect and erase pragmas
  pragmas pragmas;
  in = _filter_pragmas(in, pragmas);

  // Run extra prolog expressions
  for (const value plexpr : pragmas["prolog"])
    pl << plexpr;

  try
  {
    opi::execution_timer preprocessor_timer {"Preprocessor"};
    scheme_preprocessor pp;
    const value ppcode = pp.transform_block(in);
    preprocessor_timer.stop();

    info("\e[1mrunning Type Check on\e[0m");
    opi::execution_timer analyzer_timer {"Type analyzer"};
    size_t cnt = 0;
    const auto [out, type_map] =
        translate_to_scheme(cnt, pl, ppcode, pragmas["scheme-translator"]);
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
  catch (const bad_code &exn)
  {
    error("{}", exn.display());
    exit(EXIT_FAILURE);
  }
  catch (const std::runtime_error &exn)
  {
    error("{}", exn.what());
    exit(EXIT_FAILURE);
  }
}

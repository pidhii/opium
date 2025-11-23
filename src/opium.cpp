#include "opium/opium.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/scheme/scheme_type_system.hpp"

#include <optional>
#include <fstream>


static void
_write_scheme_script(std::ostream &os, opi::value script)
{
  for (const opi::value expr : range(script))
    os << opi::pprint_scm(opi::raw_printer, expr) << "\n\n";
}


void
opi::generate_scheme(const scheme_translator &config, value in,
                     const std::filesystem::path &opath,
                     scheme_type_location_map &tlm,
                     const std::optional<prolog_guide_function> &guide)
{
  using namespace opi;

  info("\e[1mrunning preprocessor\e[0m");
  opi::execution_timer preprocessor_timer {"Preprocessor"};
  const value ppcode = config.preprocessor.transform_block(in);
  preprocessor_timer.stop();

  info("\e[1mrunning Type Check\e[0m");
  opi::execution_timer analyzer_timer {"Type analyzer"};
  const auto [out, restlm] = translate_to_scheme(config, ppcode, tlm, guide);
  analyzer_timer.stop();

  tlm = restlm;

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

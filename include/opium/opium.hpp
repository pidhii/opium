#pragma once

#include "opium/scheme/scheme_type_system.hpp"
#include "opium/value.hpp"
#include "opium/prolog_repl.hpp"


namespace opi {

void
apply_prolog_pragmas(opi::value opiprogram, opi::prolog_repl &pl);

void
generate_scheme(const scheme_translator &config, value in,
                const std::filesystem::path &opath,
                const std::optional<prolog_guide_function> &guide = std::nullopt);

} // namespace opi
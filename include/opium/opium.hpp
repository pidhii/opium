#pragma once

#include "opium/predicate_runtime.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"
#include "opium/prolog_repl.hpp"


namespace opi {

void
apply_prolog_pragmas(opi::value opiprogram, opi::prolog_repl &pl);

void
generate_scheme(
    opi::value in, opi::scheme_preprocessor &pp, const opi::prolog &pl,
    const std::filesystem::path &opath,
    const std::optional<prolog_guide_function> &guide = std::nullopt);

} // namespace opi
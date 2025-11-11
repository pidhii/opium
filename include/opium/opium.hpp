#pragma once

#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"
#include "opium/prolog_repl.hpp"


namespace opi {

void
generate_scheme(opi::value in, opi::scheme_preprocessor &pp,
                opi::prolog_repl &pl, const std::filesystem::path &opath);

} // namespace opi
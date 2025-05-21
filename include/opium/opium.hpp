#pragma once

#include "opium/value.hpp"
#include "opium/prolog_repl.hpp"


namespace opi {

void
generate_scheme(opi::value in, opi::prolog_repl &pl,
                const std::filesystem::path &opath);

} // namespace opi
#pragma once

#include "opium/value.hpp"


void
init_readline();

void
cleanup_readline();

bool
prompt_line(const std::string &prompt, std::string &line);

void
extract_symbols(const opi::value expr);
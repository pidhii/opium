#include "opium/logging.hpp"


size_t opi::logging_indent = 0;

enum opi::loglevel opi::loglevel = opi::loglevel::silent;

std::set<std::string> opi::global_flags;
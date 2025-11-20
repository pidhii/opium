#include "opium/scheme/scheme_transformations.hpp"


static struct: opi::scheme_syntax_plugin {
  using opi::scheme_syntax_plugin::scheme_syntax_plugin;

  void
  load(opi::scheme_code_flattener &)
  { }

  void
  load(opi::scheme_unique_identifiers &)
  { }

  void
  load(opi::prolog_emitter &)
  { }
} parameterize;

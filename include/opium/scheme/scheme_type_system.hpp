#pragma once

#include "opium/code_transform_utils.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/pretty_print.hpp"
#include <ranges>


namespace opi {

static auto
scheme_type_check(size_t gensym_counter, prolog &pl, value code)
{
  const prolog_formatter plfmt;
  pretty_printer pprint_pl {plfmt};

  const scheme_formatter scmfmt;
  pretty_printer pprint_scm {scmfmt};

  // Compose translator from Scheme to Prolog
  symbol_generator genuid {gensym_counter};
  scheme_code_flattener flatten {genuid};
  scheme_unique_identifiers insert_uids {genuid};
  scheme_to_prolog to_prolog {gensym_counter};
  prolog_cleaner pl_cleaner;

  to_prolog.add_global("pair?", "pair?");
  to_prolog.add_global("cons", "cons");
  to_prolog.add_global("unpack-tuple/2", "unpack-tuple/2");
  to_prolog.add_global("unpack-pair", "unpack-pair");
  to_prolog.add_global("-", "-");
  to_prolog.add_global(">", ">");
  to_prolog.add_global("some?", "some?");
  to_prolog.add_global("real_time", "real_time");
  to_prolog.add_global("poll", "poll");
  to_prolog.add_global("values", "values");
  to_prolog.add_global("and", "and");
  to_prolog.add_global("<f>", "<f>");
  to_prolog.add_global("<z>", "num");
  to_prolog.add_global("<xs>", "<xs>");
  to_prolog.add_global("<thread>", "<thread>");

  // Translate the input expression
  const value ppcode = list(range(code)
                     | std::views::transform(std::ref(flatten))
                     | std::views::transform(std::ref(insert_uids)));
  const value plcode = list(range(to_prolog.transform_block(ppcode)) |
                            std::views::transform(std::ref(pl_cleaner)));

  const value unresolved = list(to_prolog.unresolved());
  if (length(unresolved) > 0)
  {
    for (const value symbol : range(unresolved))
    {
      error("unresolved symbol: \e[1m‘{}’\e[0m", symbol);
      source_location location;
      if (lisp_parser::get_location(symbol, location))
        std::cerr << display_location(location, 0, "\e[38;5;1;1m") << std::endl;
    }
    throw std::runtime_error {"unresolved symbols"};
  }

  // Collect predicates generated during translation
  for (const predicate &pred : to_prolog.predicates())
  {
    if (loglevel == loglevel::info)
    {
      info("add generated predicate:");
      std::cerr << std::format("{}{} :-\n  ", pred.name(),
                               list(pred.arguments()));
      pprint_pl(std::cout, pl_cleaner(pred.body()), 2);
      std::cerr << std::endl;
    }
    pl.add_predicate(pred);
  }

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                  Query resulting Prolog expression
  predicate_runtime prt;

  // Tracing of non-terminal variables
  opi::stl::unordered_set<cell *> maybe_nonterminal;
  auto trace_nonterminals = [&](predicate_runtime &, cell *c) {
    maybe_nonterminal.insert(c);
  };

  // Saving query results
  // Helper function the check if cell corresponds to non-terminal variable
  auto isnonterminal = [&maybe_nonterminal](cell *c) {
    return std::any_of(maybe_nonterminal.begin(), maybe_nonterminal.end(),
                       [c](cell *x) { return find(c) == find(x); });
  };
  opi::stl::unordered_map<value, opi::stl::unordered_set<value>> results;
  auto save_results = [&]() {
    for (const value varname : prt.variables())
    {
      // Reconstruct variable value
      const value cval = reconstruct(prt[varname], [&](cell *c) {
        // Store different markers for unbound terminal and non-terminal vars
        if (isnonterminal(c))
          return "<nonterminal>";
        else
          return "<any>";
      });
      results[varname].insert(cval);
    }
  };

  // Run the query
  const value cellularized = insert_cells(prt, plcode);
  pl.make_true(prt, cellularized, save_results, trace_nonterminals);

  return std::make_pair(results, plcode);
}

} // namespace opi
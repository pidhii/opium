#include "opium/scheme/scheme_emitter_context.hpp"


// NOTE: making effort to ensure there is only a single predicate matching
opi::predicate
opi::_find_predicate_for_template(const scheme_to_prolog &prolog_emitter,
                                  value template_name)
{
  // Matcher to match suiting predicate based on predicate signature
  const match m {
      list("result-of", template_name),
      list("result-of", cons(cons(template_name, "_"), "args"), "result")};

  // Find suiting predicate
  const auto f = [&](const auto &pred) { return m(pred.signature()); };
  opi::stl::list<predicate> preds;
  std::ranges::copy(prolog_emitter.predicates() | std::views::filter(f),
                    std::back_inserter(preds));

  assert(preds.size() == 1);
  return preds.front();
}

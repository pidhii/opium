#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"

#include <ranges>
#include <readline/readline.h>


template <typename ...T>
struct _state_saver {
  _state_saver(T &...refs): m_save {refs...}, m_refs {refs...} {}

  ~_state_saver()
  { m_refs = m_save; }

  std::tuple<T...> m_save;
  std::tuple<T&...> m_refs;
}; // struct _state_saver

opi::scheme_unique_identifiers::scheme_unique_identifiers(
    symbol_generator &gensym)
: m_gensym {gensym},
  m_alist {nil}
{
  flip_page();

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define (function-syntax)
  // Note: leaks alist to the surrounding context
  const value fndefpat = list("define", list("f", "xs", "..."), "body", "...");
  append_rule({list("define"), fndefpat}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs= ms.contains("xs") ? ms.at("xs") : nil;
    const value body = ms.contains("body") ? ms.at("body") : nil;

    // Replace identifiers with unique ones
    const value newf = m_gensym();
    m_alist = cons(cons(f, newf), m_alist); // Leak function identifier
    _state_saver _ {m_alist}; // But will roll-back further changes to alist
    value newxs = nil;
    for (const value ident : range(xs))
    {
      const value newident = m_gensym();
      newxs = append(newxs, list(newident));
      m_alist = cons(cons(ident, newident), m_alist);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("define", cons(newf, newxs), dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  // Note: leaks alist to the surrounding context
  const value defpat = list("define", "ident", "body", "...");
  append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.contains("body") ? ms.at("body") : nil;

    // Replace identifier with unique one and update alist
    const value newident = m_gensym();
    m_alist = cons(cons(ident, newident), m_alist);

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("define", newident, dot, newbody);
  });

  // Helper macro with common code for all let-expressions
#define UNPACK_MATCHES_AND_SAVE_STATE(ms)                                      \
  /* Unpack matches */                                                         \
  value idents = (ms).contains("ident") ? (ms).at("ident") : nil;              \
  value exprs = (ms).contains("expr") ? (ms).at("expr") : nil;                 \
  const value body = (ms).contains("body") ? (ms).at("body") : nil;            \
  /* Guard to recover alist after finishing this function */                   \
  _state_saver _ {m_alist};

  const value letpat = list(list(list("ident", "expr"), "..."), "body", "...");

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                let
  append_rule({list("let"), cons("let", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    // Transform bind-expressions with current alist;
    // replace identifiers with new unique symbols;
    value newbinds = nil;
    value newalist = m_alist;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);
      const value newident = m_gensym();
      newbinds = append(newbinds, list(list(newident, (*this)(expr))));
      newalist = cons(cons(ident, newident), newalist);
    }

    // Update alist with new identifiers
    m_alist = newalist;

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("let", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                let*
  append_rule({list("let*"), cons("let*", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    // Replace identifiers with new unique symbols;
    // transform bind-expressions updating alist after each binding;
    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);
      const value newident = m_gensym();
      newbinds = append(newbinds, list(list(newident, (*this)(expr))));
      m_alist = cons(cons(ident, newident), m_alist);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("let*", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                              letrec
  append_rule({list("letrec"), cons("letrec", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    // Update alist with identifiers from bindings replaced with unique symbols
    for (const value ident : range(idents))
    {
      const value newident = m_gensym();
      m_alist = cons(cons(ident, newident), m_alist);
    }

    // Transform binds with new alist
    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);
      newbinds = append(newbinds, list(list((*this)(ident), (*this)(expr))));
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("letrec", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                              letrec*
  append_rule({list("letrec*"), cons("letrec*", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    // Replace identifiers with new unique symbols;
    // update alist with new symbol before each binding
    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);
      const value newident = m_gensym();
      m_alist = cons(cons(ident, newident), m_alist);
      newbinds = append(newbinds, list(list(newident, (*this)(expr))));
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("letrec*", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const value lambdapat = list("lambda", "args", dot, "body");
  append_rule({list("lambda"), lambdapat}, [this](const auto &ms) {
    const value args = ms.at("args");
    const value body = ms.at("body");

    // Roll-back alist afterward
    _state_saver _ {m_alist};

    // Replace identifiers in arguments with unique ones
    value newargs = nil;
    for (const value ident : range(args))
    {
      const value newident = m_gensym();
      newargs = append(newargs, list(newident));
      m_alist = cons(cons(ident, newident), m_alist);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(*this));
    return list("lambda", newargs, dot, newbody);
  });

  append_rule(match {nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const value form = cons(f, xs);
    return list(range(form) | std::views::transform(*this));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atoms
  append_rule({nil, "ident"}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    if (ident->t == tag::pair)
      throw code_transformation_error {
          std::format("scheme_code_transformer rule [<ident> -> ...] - "
                      "expected atom, got {}; likely unmatched syntax",
                      ident)};
    // Lookup for identifier 
    value newident = nil;
    if (issym(ident), assoc(ident, m_alist, newident))
      return newident;
    return ident;
  });
}

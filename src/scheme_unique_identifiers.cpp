#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/value.hpp"

#include <ranges>
#include <readline/readline.h>

using namespace std::placeholders;


inline opi::value
opi::scheme_unique_identifiers::_T(value expr)
{
  utl::state_saver _ {m_is_toplevel};
  m_is_toplevel = false;
  return (*this)(expr);
}

opi::scheme_unique_identifiers::scheme_unique_identifiers(
    symbol_generator &gensym, bool is_toplevel)
: T {std::bind(&scheme_unique_identifiers::_T, this, _1)},
  m_gensym {gensym},
  m_alist {nil},
  m_is_toplevel {is_toplevel}
{
  flip_page();

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define (function-syntax)
  // NOTE: leaks alist to the surrounding context
  const value fndefpat = list("define", list("f", "xs", "..."), "body", "...");
  append_rule({list("define"), fndefpat}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs= ms.contains("xs") ? ms.at("xs") : nil;
    const value body = ms.contains("body") ? ms.at("body") : nil;

    // Replace identifiers with unique ones
    const value newf = m_is_toplevel ? sym(sym_name(f)) : m_gensym();
    copy_location(f, newf);
    m_alist = cons(cons(f, newf), m_alist); // Leak function identifier
    utl::state_saver _ {m_alist}; // But will roll-back further changes to alist
    value newxs = nil;
    for (const value ident : range(xs))
    {
      const value newident = m_gensym();
      newxs = append(newxs, list(newident));
      m_alist = cons(cons(ident, newident), m_alist);
      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list(m_is_toplevel ? "template" : "define", cons(newf, newxs), dot,
                newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  // NOTE: leaks alist to the surrounding context
  const value defpat = list("define", "ident", "body", "...");
  append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.contains("body") ? ms.at("body") : nil;

    // Replace identifier with unique one and update alist
    const value newident = m_gensym();
    m_alist = cons(cons(ident, newident), m_alist);

    // Copy original identifier location
    copy_location(ident, newident);

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list("define", newident, dot, newbody);
  });

  // Helper macro with common code for all let-expressions
#define UNPACK_MATCHES_AND_SAVE_STATE(ms)                                      \
  /* Unpack matches */                                                         \
  value idents = (ms).contains("ident") ? (ms).at("ident") : nil;              \
  value exprs = (ms).contains("expr") ? (ms).at("expr") : nil;                 \
  const value body = (ms).contains("body") ? (ms).at("body") : nil;            \
  /* Guard to recover alist after finishing this function */                   \
  utl::state_saver _ {m_alist};

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
      newbinds = append(newbinds, list(list(newident, T(expr))));
      newalist = cons(cons(ident, newident), newalist);

      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Update alist with new identifiers
    m_alist = newalist;

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
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
      newbinds = append(newbinds, list(list(newident, T(expr))));
      m_alist = cons(cons(ident, newident), m_alist);

      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
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

      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Transform binds with new alist
    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);
      newbinds = append(newbinds, list(list(T(ident), T(expr))));
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
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
      newbinds = append(newbinds, list(list(newident, T(expr))));

      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list("letrec*", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                              let-values
  append_rule({list("let-values"), cons("let-values", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    // Transform bind-expressions with current alist;
    // replace identifiers with new unique symbols;
    value newbinds = nil;
    value newalist = m_alist;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value identlist = car(idents);
      const value expr = car(exprs);

      value newidentlist = nil;
      for (const value ident : range(identlist))
      {
        const value newident = m_gensym();
        newidentlist = append(newidentlist, list(newident));
        newalist = cons(cons(ident, newident), newalist);

        // Copy original identifier location
        copy_location(ident, newident);
      }
      newbinds = append(newbinds, list(list(newidentlist, T(expr))));
    }

    // Update alist with new identifiers
    m_alist = newalist;

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list("let-values", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                              let*-values
  append_rule({list("let*-values"), cons("let*-values", letpat)}, [this](const auto &ms) {
    UNPACK_MATCHES_AND_SAVE_STATE(ms)

    value newbinds = nil;
    for (; exprs->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value identlist = car(idents);
      const value expr = car(exprs);

      const value newexpr = T(expr);

      value newidentlist = nil;
      for (const value ident : range(identlist))
      {
        const value newident = m_gensym();
        m_alist = cons(cons(ident, newident), m_alist);
        newidentlist = append(newidentlist, list(newident));

        // Copy original identifier location
        copy_location(ident, newident);
      }
      newbinds = append(newbinds, list(list(newidentlist, newexpr)));
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list("let*-values", newbinds, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const value lambdapat = list("lambda", "args", dot, "body");
  append_rule({list("lambda"), lambdapat}, [this](const auto &ms) {
    const value args = ms.at("args");
    const value body = ms.at("body");

    // Roll-back alist afterward
    utl::state_saver _ {m_alist};

    // Replace identifiers in arguments with unique ones
    value newargs = nil;
    for (const value ident : range(args))
    {
      const value newident = m_gensym();
      newargs = append(newargs, list(newident));
      m_alist = cons(cons(ident, newident), m_alist);

      // Copy original identifier location
      copy_location(ident, newident);
    }

    // Transform body with new alist
    const value newbody = list(range(body) | std::views::transform(T));
    return list("lambda", newargs, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  append_rule(match {nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const value form = cons(f, xs);
    return list(range(form) | std::views::transform(T));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atoms
  append_rule({nil, "ident"}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    if (ident->t == tag::pair)
      throw code_transformation_error {
          std::format("scheme_code_transformer rule [<ident> -> ...] - "
                      "expected atom, got {}; likely unmatched syntax",
                      ident),
          ident};
    // Lookup for identifier 
    value newident = nil;
    if (issym(ident) and assoc(ident, m_alist, newident))
      return sym(sym_name(newident)); // Copy it for pointer-based code tracking
    return ident;
  });
}

/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/value.hpp"

#include <ranges>
#include <readline/readline.h>
#include <utility>

using namespace std::placeholders;


inline opi::value
opi::scheme_unique_identifiers::_T(value expr)
{ return (*this)(expr); }

opi::scheme_unique_identifiers::scheme_unique_identifiers(
    symbol_generator &gensym)
: T {std::bind(&scheme_unique_identifiers::_T, this, _1)},
  m_gensym {gensym},
  m_alist {nil},
  m_overload_alist {nil}
{
  flip_page();

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                  define-overload (function-syntax)
  const value fndefovldpat =
      list("define-overload", cons("identifier", "xs"), dot, "body");
  append_rule({list("define-overload"), fndefovldpat}, [this](const auto &ms) {
    const value identifier = ms.at("identifier");
    const value xs = ms.at("xs");
    const value body = ms.at("body");

    // Get the mapped identifier created during forward-declaration
    value newidentifier = nil;
    const bool ok = assq(identifier, m_overload_alist, newidentifier);
    assert(ok && "Missing overload identifier");
    copy_location(identifier, newidentifier);

    // Roll-back further changes to alist
    utl::state_saver _ {m_alist};

    // Replace function arguments with unique identifiers
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
    const value newbody = transform_block(body);
    return list("template", cons(newidentifier, newxs), dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                      define (function-syntax)
  // TODO: merge with define-overload as the handlers are identical
  const value fndefpat = list("define", cons("identifier", "xs"), dot, "body");
  append_rule({list("define"), fndefpat}, [this](const auto &ms) {
    const value identifier = ms.at("identifier");
    const value xs = ms.at("xs");
    const value body = ms.at("body");

    // Get the mapped identifier created during forward-declaration
    const value newidentifier = _copy_mapped_identifier(identifier);
    copy_location(identifier, newidentifier);

    // Roll-back changes to alist after this point
    utl::state_saver _ {m_alist};

    // Replace function arguments with unique identifiers
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
    const value newbody = transform_block(body);
    return list("template", cons(newidentifier, newxs), dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  const value defpat = list("define", "identifier", dot, "body");
  append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value identifier = ms.at("identifier");
    const value body = ms.at("body");

    // Get the mapped identifier created during forward-declaration
    const value newidentifier = _copy_mapped_identifier(identifier);
    copy_location(identifier, newidentifier);

    // Transform body with new alist
    const value newbody = transform_block(body);
    return list("define", newidentifier, dot, newbody);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
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
    const value newbody = transform_block(body);
    return list("lambda", newargs, dot, newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               begin
  const value beginpat = cons("begin", "body");
  append_rule({list("begin"), beginpat}, [this](const auto &ms) {
    const value body = ms.at("body");
    const value newbody = transform_block(body);
    return cons("begin", newbody);
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


opi::value
opi::scheme_unique_identifiers::_copy_mapped_identifier(value identifier) const
{
  value mappedidentifier = nil;
  const bool ok = assoc(identifier, m_alist, mappedidentifier);
  assert(ok && "Can't find identifier in a-list");
  return sym(sym_name(mappedidentifier));
}


opi::value
opi::scheme_unique_identifiers::transform_block(value block) const
{
  const match define_overload {
    list("define-overload"),
    list("define-overload", cons("identifier", "xs"), dot, "body")
  };

  const match define_function {
    list("define"),
    list("define", cons("identifier", "xs"), dot, "body")
  };

  const match define_identifier {
    list("define"),
    list("define", "identifier", dot, "body")
  };

  const auto try_match = [&](value expr, const match &m, value &identifier) {
    opi::stl::unordered_map<value, value> matches;
    if (m(expr, matches))
    {
      assert(matches.contains("identifier"));
      identifier = matches.at("identifier");
      return true;
    }
    return false;
  };

  // Recover a-list after the block is processed
  utl::state_saver _ {m_alist, m_overload_alist};

  // Each overloaded identifier must belong to an overload-group. Definitions
  // of overloaded functions use unique identifiers as all other functions.
  // However, any references to the overloaded identifiers are to be done via
  // a special identifier of the corresponding overload group.
  struct overload_group {
    overload_group(): group_identifier {nil} { }
    value group_identifier;
    opi::stl::unordered_set<value> templates;
  };
  opi::stl::unordered_map<value, overload_group> ovgroups;

  // Storage for identifiers of (non-overloaded) templates for forward declaration
  opi::stl::unordered_set<value> templates;

  // Forward-declarations for all define-family syntaxes
  for (const value expr : range(block))
  {
    value identifier = nil;
    if (try_match(expr, define_overload, identifier))
    {
      // Check if apropriate overload group exists and crate it if not.
      auto it = ovgroups.find(identifier);
      if (it == ovgroups.end())
      { // Create new overload group
        it = ovgroups.emplace(std::piecewise_construct,
                              std::forward_as_tuple(identifier),
                              std::forward_as_tuple())
                     .first;
        const value groupidentifier = m_gensym();
        it->second.group_identifier = groupidentifier;
        m_alist = cons(cons(identifier, groupidentifier), m_alist);
      }

      // Generate unique identifier for this definition
      const value newidentifier = m_gensym();
      m_overload_alist = cons(cons(identifier, newidentifier), m_overload_alist);

      // Add `newidentifier` to the overload group.
      overload_group &group = it->second;
      const bool ok = group.templates.emplace(newidentifier).second;
      assert(ok && "Failed to add identifier to an overload group");
    }
    else if (try_match(expr, define_function, identifier) or
             try_match(expr, define_identifier, identifier))
    {
      const value newidentifier = m_gensym();
      m_alist = cons(cons(identifier, newidentifier), m_alist);

      // Add `newidentifier` to a list of templates
      templates.emplace(newidentifier);
    }
  }

  value header = nil;

  // Declare overloads
  for (const overload_group &group : ovgroups | std::views::values)
  {
    for (const value templateident : group.templates)
    {
      const value decl = list("declare-template-overload",
                              group.group_identifier, templateident);
      header = cons(decl, header);
    }
  }

  // Declare other templates
  for (const value templateident : templates)
  {
    const value decl = list("declare-template", templateident);
    header = cons(decl, header);
  }

  // Once forward-declarations are handled do the actual transformation
  const value newblock =
      list(range(block) | std::views::transform(std::ref(*this)));

  return append(header, newblock);
}
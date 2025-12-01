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

#include "opium/scheme/translator/scheme_emitter.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/scheme/translator/exceptions.hpp"
#include "opium/scheme/translator/scheme_emitter_context.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/ranges.hpp"
#include "opium/value.hpp"


opi::value
opi::scheme_emitter::_unfold_types(opi::value x) const
{
  if (ispair(x))
    return cons(_unfold_types(car(x)), _unfold_types(cdr(x)));
  else
  {
    const auto it = m_type_bindings.find(x);
    if (it != m_type_bindings.end())
    {
      assert(it->second.size() == 1);
      return *it->second.begin();
    }
    else
      return x;
  }
}


opi::value
opi::scheme_emitter::_unfold_pattern_type(opi::value pattern) const
{
  if (ispair(pattern))
  {
    const opi::value ctor = car(pattern);
    const opi::value args = cdr(pattern);

    const opi::value newctor = _unfold_types(ctor);
    opi::value newargs = opi::nil;
    for (const opi::value arg : range(args))
      newargs = append(newargs, list(_find_code_type(arg)));
    return cons(newctor, newargs);
  }
  else
    return pattern;
}


opi::scheme_emitter::scheme_emitter(scheme_emitter_context &ctx,
                                    const type_bindings &query)
: m_dont_emit_symbol {"<dont-emit>"}, m_type_bindings {query}, m_ctx {ctx}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                         other pragmas (omit)
  m_transformer.prepend_rule(
      {list("pragma"), list("pragma", dot, "_")},
      [this](const auto &) { return m_dont_emit_symbol; });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                         inline-scheme
  m_transformer.prepend_rule(
      {list("pragma", "inline-scheme"), list("pragma", "inline-scheme", "expr")},
      [](const auto &ms) { return ms.at("expr"); });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               annotate-type
  const match asstypematch {list("annotate-type"),
                            list("annotate-type", "expr", dot, "types")};
  m_transformer.prepend_rule(asstypematch, [this](const auto &ms) {
    const value expr = ms.at("expr");

    // TODO:
    // o handle non-nop type coercions
    // Just forward the `expr`
    return m_transformer(expr);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                        declare-template-overload
  m_transformer.prepend_rule(
      {list("declare-template-overload"),
       list("declare-template-overload", "ovident", "_")},
      [this](const auto &ms) {
        m_ctx.register_identifier_for_function_template(ms.at("ovident"));
        return m_dont_emit_symbol;
      });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                           declare-template
  m_transformer.prepend_rule(
      {list("declare-template"), list("declare-template", "_")},
      [this](const auto &) { return m_dont_emit_symbol; });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                              declare
  m_transformer.prepend_rule(
      {list("declare"), list("declare", "_")},
      [this](const auto &) { return m_dont_emit_symbol; });
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 cases
  const match casesmatch {
      list("cases"), list("cases", "exprs", cons("patterns", "branch"), "...")};
  m_transformer.prepend_rule(casesmatch, [this](const auto &ms) {
    const value exprs = ms.at("exprs");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;

    const value newexprs = transform_list(exprs);

    value condclauses = nil;
    for (const auto &[rowpatterns, branch] :
         utl::zip(range(patterns), range(branches)))
    {
      value conditions = nil;
      value valuesbindings = nil;
      for (const auto [pattern, expr] :
           utl::zip(range(rowpatterns), range(newexprs)))
      {
        // Use predicate test for every non-wildcard pattern, and bind everything
        // with let-values
        if (ispair(pattern))
        {
          const value exprtype = _find_code_type(expr);
          const value patterntype = _unfold_pattern_type(pattern);
          const match_translation_rules::rule &rule =
              m_ctx.mtr().find_rule(patterntype, exprtype);
          // Create new condition to guard the branch
          const value newcond = list(rule.predicate, expr);
          conditions = append(conditions, list(newcond));
          // Create new bindings with unpack rule
          if (cdr(pattern) != nil)
          {
            const value newvaluesbind = list(cdr(pattern), list(rule.unpack, expr));
            valuesbindings = append(valuesbindings, list(newvaluesbind));
          }
        }
        else
        {
          const value newvaluesbind = list(list(pattern), expr);
          valuesbindings = append(valuesbindings, list(newvaluesbind));
        }
      }

      // Build final test condition
      const value test = conditions == nil         ? True
                         : length(conditions) == 1 ? car(conditions)
                                                   : cons("and", conditions);

      // Build brnach body wirapping initial expression into let-values (if needed)
      value newbranch = transform_block(branch);
      if (valuesbindings != nil)
        newbranch = list(list("let-values", valuesbindings, dot, newbranch));

      // Create an entry for `cond`-expression
      const value condclause = cons(test, newbranch);
      condclauses = append(condclauses, list(condclause));
    }

    return cons("cond", condclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 template
  const value temppat = list("template", cons("ident", "parms"), dot, "body");
  const auto temprule = [this](const auto &ms, value fm) {
    const value identifier = ms.at("ident");
    const value params = ms.at("parms");
    const value body = ms.at("body");

    // FIXME (ugly)
    value fixedparams = nil;
    for (const value param : range(params))
      fixedparams = append(fixedparams, list(issym(param) ? param : car(param)));
    const value cleandef =
        list("template", cons(identifier, fixedparams), dot, body);

    // Register this identifier as a reference to a function template 
    // NOTE: concrete overload referenced by the identifier is subject to be
    // be determined by the TypeCheck; this registration needed just to remember
    // to substitute this particular identifier with another identifier refering
    // to an apropriate specialization
    m_ctx.register_identifier_for_function_template(identifier);

    // Register all the pieces of information need for generation of function
    // template specializations in future
    const value typetemplate = _find_code_type(fm);
    m_ctx.register_template(identifier, {cleandef, typetemplate, m_ctx});

    return m_dont_emit_symbol;
  };
  m_transformer.prepend_rule({list("template"), temppat}, temprule);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "parms", dot, "body")};
  m_transformer.prepend_rule(lambdamatch, [this](const auto &ms, value fm) {
    const value parms = ms.at("parms");
    const value body = ms.at("body");

    const value type_template = _find_code_type(fm);
    const value type = _find_code_type(car(fm));

    const value specialbody = generate_function_template_body(
        m_ctx, type, type_template, body);

    // Return normal Scheme lambda expression but with specialized body
    return list("lambda", parms, dot, specialbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                begin
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  m_transformer.prepend_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    // FIXME: maybe have to involve `transform_block()` here
    return transform_inner_block_into_expression(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                       let-values/let*-values
#if 1
  const auto valuesrule = [this](const auto &ms, value fm) {
    const value binds = ms.at("binds");
    const value body = ms.at("body");

    // Insert a wildcard placeholder at the end of every sequence of values:
    // Scheme requires that the list of values matches actual return of the expr,
    // meanwhile the TypeCheck of Opium allows for fewer LHS values than that in
    // return of the RHS expr
    value newbinds = nil;
    for (const value bind : range(binds))
    {
      const value idents = car(bind);
      const value expr = car(cdr(bind));

      const value newidents = append(idents, "_");
      const value newexpr = m_transformer(expr);
      newbinds = append(newbinds, list(list(newidents, newexpr)));
    }

    // Transform the body
    const value newbody = transform_block(body);
        // list(range(body) | std::views::transform(std::ref(m_transformer)));

    return list(car(fm), newbinds, dot, newbody);
  };

  const match letvalsmatch {list("let-values"),
                            list("let-values", "binds", dot, "body")};
  m_transformer.prepend_rule(letvalsmatch, valuesrule);

  const match letstarvalsmatch {list("let*-values"),
                                list("let*-values", "binds", dot, "body")};
  m_transformer.prepend_rule(letstarvalsmatch, valuesrule);
#endif

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  m_transformer.append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const value form = cons(f, xs);
    // TODO:
    // o handle non-nop type coercions
    return list(range(form) | std::views::transform(std::ref(m_transformer)));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atom
  m_transformer.append_rule({nil, "x"}, [this](const auto &ms) {
    const value x = ms.at("x");

    // Get type of this atom
    value type = nil;
    if (not m_ctx.ctm().code_type(x, type))
      throw code_transformation_error {
          std::format("Don't know what type to use for {}", x), x};

    // Get deduced type if it is not given explicitly
    const auto it = m_type_bindings.find(type);
    if (it != m_type_bindings.end())
    {
      const auto &possibilities = it->second;
      if (possibilities.size() != 1)
      {
        source_location location;
        std::ostringstream buf;
        if (get_location(x, location))
          buf << "Ambiguous type: "
              << display_location(location, 2, "\e[38;5;9m", "\e[2m");
        else
          buf << std::format("Ambiguous type {} for {} ({} options):\n", type, x, possibilities.size());
        for (size_t iopt = 1; const value variant : possibilities)
        {
          if (get_location(variant, location))
            buf << "option " << iopt << ": "
                << display_location(location, 1, "\e[38;5;4;1m", "\e[2m");
          else if (ispair(variant) and
                   car(variant) == "#dynamic-function-dispatch" and
                   get_location(car(cdr(variant)), location))
          {
            buf << "option " << iopt << ": "
                << display_location(location, 1, "\e[38;5;4;1m", "\e[2m");
            buf << std::format("type: {:c#5}\n", variant);
          }
          else
            buf << "option " << iopt << ": " << variant << "\n";
          iopt += 1;
        }
        throw ambiguous_type_error {buf.str()};
      }
      type = *possibilities.begin();
    }

    // Instantiate function template
    if (m_ctx.identifier_refers_to_function_template(x))
      return instantiate_function_template(m_ctx, type);

    // TODO:
    // o handle non-trivial coercions
    if (issym(x))
      return x;
    else
      return m_ctx.lc()(x, type);
  });
}

opi::value
opi::scheme_emitter::_find_code_type(value code) const
{
  const value type = m_ctx.ctm().code_type(code);
  const auto it = m_type_bindings.find(type);
  if (it != m_type_bindings.end())
  {
    const auto &possibilities = it->second;
    if (possibilities.size() != 1)
      throw bad_code {
          std::format("Ambiguous type: {}", list(possibilities)), code};
    return *possibilities.begin();
  }
  else
    return type;
}


// Fix for "define in expression context" error
opi::value
opi::scheme_emitter::transform_inner_block_into_expression(value block)
{
  // Transform all expressions within the block
  const opi::value tblock = transform_list(block);

  if (tblock == nil)
    return nil;

  if (length(tblock) == 1)
    return car(tblock);

  // Separate last expression from the rest
  const value revblock = reverse(tblock);
  const value last = car(revblock);
  const value head = reverse(cdr(revblock));

  // Wrap everything into a letrec*-expression
  value binds = nil;
  for (size_t cnt = 0; const value expr : range(head))
  {
    value newbinds = nil;
    if (ispair(expr) and car(expr) == "define")
    {
      const value sign = car(cdr(expr));
      const value body = cdr(cdr(expr));

      if (issym(sign))
        newbinds = list(list(sign, length(body) > 1 ? cons("begin", body) : car(body)));
      else
      {
        const value ident = car(sign);
        const value args = cdr(sign);
        newbinds = list(list(ident, list("lambda", args, body)));
      }
    }
    else if (ispair(expr) and car(expr) == "define-values")
    { // Embedding of define-values into letrec*:
      // - for each variable create a "dummy" binding to allocate the variable
      //   and bind it to #<unsepcified> value
      // - and then use set-values! syntax to assign the variables
      //
      // It would look like:
      //     (letrec ((x x)  ;; declare x
      //              (y y)  ;; declare y
      //              (_ (set-values! (x y) ...))) ;; bind x and y -variables
      //        ...)
      const value idents = car(cdr(expr));
      const value body = cdr(cdr(expr));
      // create declarations
      for (const value ident : range(idents))
      {
        const value decl = list(ident, ident);
        newbinds = append(newbinds, list(decl));
      }
      // create assignment
      const value dummy = sym(std::format("_{}", cnt++));
      const value assign = list("set!-values", idents, list("begin", dot, body));
      const value letrecclause = list(dummy, assign);
      newbinds = append(newbinds, list(letrecclause));
    }
    else
      newbinds = list(list(sym(std::format("_{}", cnt++)), expr));

    binds = append(binds, newbinds);
  }

  return list("letrec*", binds, last);
}

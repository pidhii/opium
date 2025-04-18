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


#include "opium/scheme/scheme_emitter.hpp"
#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/value.hpp"
#include "opium/utilities/ranges.hpp"


opi::scheme_emitter::scheme_emitter(scheme_emitter_context &ctx, query_result &query)
: m_dont_emit_symbol {"<dont-emit>"}, m_query_result {query}, m_ctx {ctx}
{
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
      for (const auto [pattern, expr] : utl::zip(range(rowpatterns), range(newexprs)))
      {
        // Use predicate test for every non-wildcard pattern, and bind everything
        // with let-values
        if (pattern->t == tag::pair)
        {
          const value exprtype = _find_code_type(expr);
          const case_to_scheme &rule = m_ctx.find_case_rule(pattern, exprtype);
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
      value newbranch = transform_list(branch);
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

    // Register this identifier as a reference to a function template 
    // NOTE: concrete overload referenced by the identifier is subject to be
    // be determined by the TypeCheck; this registration needed just to remember
    // to substitute this particular identifier with another identifier refering
    // to an apropriate specialization
    m_ctx.register_identifier_for_function_template(identifier);

    // Register all the pieces of information need for generation of function
    // template specializations in future
    const value typetemplate = _find_code_type(fm);
    m_ctx.register_template(identifier, {fm, typetemplate, m_ctx});

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
  //                                form
  m_transformer.append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const value form = cons(f, xs);
    return list(range(form) | std::views::transform(std::ref(m_transformer)));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atom
  m_transformer.append_rule({nil, "x"}, [this](const auto &ms) {
    const value x = ms.at("x");

    // Get type of this atom
    value type = nil;
    if (not m_ctx.prolog_emitter().find_code_type(x, type))
      throw code_transformation_error {
          std::format("Don't know what type to use for {}", x), x};

    // Get deduced type if it is not given explicitly
    const auto it = m_query_result.find(type);
    if (it != m_query_result.end())
    {
      const auto &possibilities = it->second;
      if (possibilities.size() != 1)
        throw code_transformation_error {
            std::format("Ambiguous type for {}: {}", x, list(possibilities)), x};
      type = *possibilities.begin();
    }

    // instantiate
    if (m_ctx.identifier_refers_to_function_template(x))
      return instantiate_function_template(m_ctx, type);
    else
      return x;

    return x;
  });
}

opi::value
opi::scheme_emitter::_find_code_type(value code) const
{
  const value type = m_ctx.prolog_emitter().find_code_type(code);
  const auto it = m_query_result.find(type);
  if (it != m_query_result.end())
  {
    const auto &possibilities = it->second;
    if (possibilities.size() != 1)
      throw code_transformation_error {
          std::format("Ambiguous type: {}", list(possibilities)), code};
    return *possibilities.begin();
  }
  else
    return type;
}

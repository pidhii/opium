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
#include "opium/source_location.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/logging.hpp"
#include "opium/value.hpp"
#include "opium/utilities/ranges.hpp"
#include "opium/lisp_parser.hpp"
#include <iterator>


using namespace std::placeholders;


void
opi::scheme_to_prolog::set_up_prolog(prolog &pl) const noexcept
{
  // Add a predicate for dynamic function specialization
  lisp_parser parser;
  {
    const value signature = parser.parse(R"(
      (result-of ((#dynamic-function-dispatch _ Args Results Body) . Args) . Results)
    )");
    const value rule = parser.parse(R"(
      (call Body)
    )");
    pl.add_predicate(signature, rule);
  }
}


opi::scheme_to_prolog::scheme_to_prolog(size_t &counter,
                                        type_format_string format)
: m_type_format {format},
  m_is_template {false},
  m_targets {"_"},
  m_alist {nil},
  m_global_alist {nil},
  m_lambda_gensym {counter, "Lambda{}"}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               annotate-type
  const match asstypematch {list("annotate-type"),
                            list("annotate-type", "expr", "type")};
  append_rule(asstypematch, [this](const auto &ms) {
    const value expr = ms.at("expr");
    const value type = ms.at("type");

    // Evaluate `expr` with target set to `type`
    const value plexpr = ({
      utl::state_saver _ {m_targets};
      m_targets = list(type, dot, "_");
      (*this)(expr);
    });

    // Don't emit extra code for dummy target
    if (m_targets == "_")
      return plexpr;

    // Otherwise, also bind current target with the `type`
    const value bindtarget = list("=", m_targets, list(type));
    return list("and", plexpr, bindtarget);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 cases
  const match casesmatch {
      list("cases"), list("cases", "exprs", cons("patterns", "branch"), "...")};
  append_rule(casesmatch, [this, &counter](const auto &ms) {
    const value exprs = ms.at("exprs");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;

    // Save original target
    const value origtargets = m_targets;

    utl::state_saver _ {m_targets};

    // Translate all expressions
    opi::stl::vector<value> exprproxies;
    value plexprs = nil;
    for (const value expr : range(exprs))
    {
      const value exprproxy = sym(std::format("CaseProxy_{}", counter++));
      m_targets = list(exprproxy, dot, "_");
      const value plexpr = (*this)(expr);
      plexprs = append(plexprs, list(plexpr));
      exprproxies.push_back(exprproxy);
    }

    // Translate cases
    value plcases = nil;
    for (const auto &[rowpatterns, branch] :
         utl::zip(range(patterns), range(branches)))
    {
      utl::state_saver _ {m_alist, m_targets};

      // Process each pattern in the row
      value plmatches = nil;
      for (const auto &[pattern, exprproxy] :
           utl::zip(range(rowpatterns), exprproxies))
      {
        if (pattern->t == tag::pair)
        { // Generate patter matching via match-on predicate
          const value constructor = car(pattern);
          const value arguments = cdr(pattern);
          value plarguments = nil;
          for (const value ident : range(arguments))
          {
            const value type = _generate_type_and_copy_location(ident);
            m_alist = cons(cons(ident, type), m_alist);
            plarguments = append(plarguments, list(type));
          }
          const value plpattern = cons(constructor, plarguments);
          const value matchexpr = list("match-on", plpattern, exprproxy);
          plmatches = append(plmatches, list(matchexpr));
        }
        else
        { // Just an alias
          m_alist = cons(cons(pattern, exprproxy), m_alist);
        }
      }

      m_targets = origtargets;
      const value plbranch = transform_block(branch);

      // Add all matches and the branch to plcases
      plcases = append(plcases, plmatches);
      plcases = append(plcases, list(plbranch));
    }

    // Combine all expressions and cases
    return cons("and", append(plexprs, plcases));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 if
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  append_rule(ifmatch, [this](const auto &ms) {
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");
    const value elsebr = ms.at("else");


    // <cond> must evaluate into boolean
    const value newcond = ({
      utl::state_saver _ {m_targets};
      m_targets = list("bool", dot, "_");
      (*this)(cond);
    });

    const value newthen = (*this)(thenbr);
    const value newelse = (*this)(elsebr);

    return list("and", newcond, newthen, newelse);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 if (no else-branch)
  const match ifnoelsematch {list("if"), list("if", "cond", "then")};
  append_rule(ifnoelsematch, [this](const auto &ms, value fm) {
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");

    // <cond> must evaluate into boolean
    const value newcond = ({
      utl::state_saver _ {m_targets};
      m_targets = list("bool", dot, "_");
      (*this)(cond);
    });

    const value newthen = (*this)(thenbr);

    // Since there is no <else> branch this expression is only valid with a
    // wildcard target
    if (m_targets != "_")
    {
      throw bad_code {
          std::format(
              "Can't evaluate else-less if-expression with exact target ({})",
              m_targets),
          fm};
    }

    return list("and", newcond, newthen);
  });


  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                     let / let* / letrec / letrec*
  // Common function for all let-family syntax variants
  auto letrule = [this](const auto &ms) {
    value result = nil;

    // Roll-back changes to the a-list once done
    utl::state_saver _ {m_alist};

    // Process binds
    value idents = ms.contains("ident") ? ms.at("ident") : nil;
    value exprs = ms.contains("expr") ? ms.at("expr") : nil;
    for (; idents->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value ident = car(idents);
      const value expr = car(exprs);

      // Generate type name and add resulting identifier+type entry to the a-list
      const value type = _generate_type_and_copy_location(ident);
      m_alist = cons(cons(ident, type), m_alist);

      // Transform expression with target set to the identifier type and
      // accumulate resulting Prolog expression to the `result`
      // and accumulate transformed expression in results
      utl::state_saver _ {m_targets};
      m_targets = list(type, dot, "_");
      result = append(result, list((*this)(expr)));
    }

    // Process body and accumulate in results
    result = cons(transform_block(ms.at("body")), result);

    // Reverse results as we were accumulating from the front
    return cons("and", reverse(result));
  };

  // Add rules
  const value letpat = list(list(list("ident", "expr"), "..."), dot, "body");
  append_rule({list("let"), cons("let", letpat)}, letrule);
  append_rule({list("let*"), cons("let*", letpat)}, letrule);
  append_rule({list("letrec"), cons("letrec", letpat)}, letrule);
  append_rule({list("letrec*"), cons("letrec*", letpat)}, letrule);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                       let-values / let*-values
  // Common function for both let-values syntax variants
  auto letvalsrule = [this](const auto &ms) {
    value result = nil;

    // Roll-back changes to the a-list once done
    utl::state_saver _ {m_alist};

    // Process binds
    value idents = ms.contains("ident") ? ms.at("ident") : nil;
    value exprs = ms.contains("expr") ? ms.at("expr") : nil;
    for (; idents->t == tag::pair; idents = cdr(idents), exprs = cdr(exprs))
    {
      const value subidents = car(idents);
      const value expr = car(exprs);

      value types = "_";
      // Generate type name and add resulting identifier+type entry to the a-list
      for (const value ident : range(subidents))
      {
        const value type = _generate_type_and_copy_location(ident);
        types = append(types, list(type));
        m_alist = cons(cons(ident, type), m_alist);
      }

      // Transform expression with target set to the (values type ...*) and
      // accumulate resulting Prolog expression to the `result`
      utl::state_saver _ {m_targets};
      m_targets = types;
      result = append(result, list((*this)(expr)));
    }

    // Process body and accumulate in results
    result = cons(transform_block(ms.at("body")), result);

    // Reverse results as we were accumulating from the front
    return cons("and", reverse(result));
  };

  // Add rules
  append_rule({list("let-values"), cons("let-values", letpat)}, letvalsrule);
  append_rule({list("let*-values"), cons("let*-values", letpat)}, letvalsrule);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                        declare-template-overload
  // NOTE: leaks a-list
  const value ovdeclpat = list("declare-template-overload", "ovident", "ident");
  append_rule({list("declare-template-overload"), ovdeclpat},
              [this](const auto &ms, [[maybe_unused]]value fm) {
    const value ovident = ms.at("ovident");
    const value ident = ms.at("ident");

    // Generate types
    const value typevar = _generate_type_and_copy_location(ident);
    const value type = cons("#template", typevar);
    m_alist = cons(cons(ovident, type), m_alist);
    copy_location(ident, type);
    // if (not has_location(type))
    // {
    //   warning("Failed to assign location to function template");
    //   source_location location;
    //   if (get_location(fm, location))
    //     warning("{}", display_location(location));
    //   else if (get_location(ovident, location))
    //     warning("{}", display_location(location));
    //   else if (get_location(ident, location))
    //     warning("{}", display_location(location));
    // }

    return list("and");
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                        declare-template
  // NOTE: leaks a-list
  const value declpat = list("declare-template", "ident");
  append_rule({list("declare-template"), declpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");

    // Generate types
    const value typevar = _generate_type_and_copy_location(ident);
    const value type = cons("#template", typevar);
    m_alist = cons(cons(ident, type), m_alist);
    copy_location(ident, type);

    return list("and");
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             declare
  // NOTE: leaks a-list
  const value declonepat = list("declare", "ident");
  append_rule({list("declare"), declonepat}, [this](const auto &ms) {
    const value ident = ms.at("ident");

    // Generate types
    const value type = _generate_type_and_copy_location(ident);
    m_alist = cons(cons(ident, type), m_alist);
    copy_location(ident, type);

    return list("and");
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             template
  // NOTE: leaks a-list
  const value temppat = list("template", cons("ident", "params"), dot, "body");
  append_rule({list("template"), temppat}, [this](const auto &ms, value fm) {
    const value ident = ms.at("ident");
    const value params = ms.at("params");
    const value body = ms.at("body");

    assert(sym_name(ident)[0] != '_' and not isupper(sym_name(ident)[0]) &&
           "Tempalte identifiers can't look like Prolog variables");
    const value proxyvar = _generate_type_and_copy_location(ident);
    _link_code_to_type(fm, proxyvar);

    // Will revert a-list from any changes after this point.
    utl::state_saver _ {m_alist, m_targets, m_is_template};
    m_is_template = true;

    // Mark beginning of the nesting by inserting a NIL value into the alist
    m_alist = cons(nil, m_alist);

    // Translate function parameters into (local) type variables
    value plparams = nil;
    for (value x : range(params))
    {
      value xtype;
      if (issym(x))
        xtype = _generate_type_and_copy_location(x);
      else
      {
        xtype = car(cdr(x));
        x = car(x);
      }
      plparams = cons(xtype, plparams);
      m_alist = cons(cons(x, xtype), m_alist);
    }
    plparams = reverse(plparams); // We were inserting at front, so now reverse

    // (local) Type variable to hold function return type
    // FIXME: why does it have to generate different identifiers?
    const value plresults = sym("Results");

    // Translate the body with apropriate target
    m_targets = plresults;
    const value plbody = transform_block(body);

    // Wrapp it up. Instances of this template can now be created by simple
    // calling `insert-cells` on the structure below
    // NOTE:
    // - quasiquote to prevent evaluation of all the type variables straight away
    // - variables from the higher level(s) will be captured via quasiquote(s)
    const value function_template =
        list("quasiquote", list("#dynamic-function-dispatch", ident, plparams,
                                plresults, plbody));
    copy_location(fm, function_template);

    return list("=", proxyvar, function_template);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "params", dot, "body")};
  append_rule(lambdamatch, [this, &counter](const auto &ms, value fm) {
    const value params = ms.at("params");
    const value body = ms.at("body");

    // Save current target
    const value targets = m_targets;

    // Will revert a-list from any changes after this point
    utl::state_saver _ {m_alist, m_targets, m_is_template};
    m_is_template = true;

    // Mark beginning of the nesting by inserting a NIL value into the alist
    m_alist = cons(nil, m_alist);

    // Translate function parameters into (local) type variables
    value plparams = nil;
    for (const value x : range(params))
    {
      const value xtype = _generate_type_and_copy_location(x);
      plparams = append(plparams, list(xtype));
      m_alist = cons(cons(x, xtype), m_alist);
    }

    // (local) Type variable to hold function return type
    // FIXME: why does it have to generate different identifiers?
    const value plresults = sym("Results");

    // Translate the body with apropriate target
    m_targets = plresults;
    const value plbody = transform_block(body);

    // Generate unique identifier for this lambda
    const value lambdaname = sym(std::format("anonymous-lambda.{}", counter++));
    const value functemplate =
        list("quasiquote", list("#dynamic-function-dispatch", lambdaname,
                                plparams, plresults, plbody));

    const value proxyvar = m_lambda_gensym();

    // TODO: this is ugly
    _link_code_to_type(fm, proxyvar);
    _link_code_to_type(car(fm), car(targets)); // FIXME

    return list("and", list("=", proxyvar, functemplate),
                list("insert-cells", list(proxyvar), targets));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  // NOTE: leaks a-list
  const value defpat = list("define", "ident", dot, "body");
  append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.at("body");

    // Add identifier to the alist
    const value type = _generate_type_and_copy_location(ident);

    // Set new type as target for body evaluation
    utl::state_saver _ {m_alist, m_targets};
    m_targets = list(type, dot, "_");
    return transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                            define-values
  // NOTE: leaks a-list
  const value defvalspat = list("define-values", "idents", dot, "body");
  append_rule({list("define-values"), defvalspat}, [this](const auto &ms) {
    const value idents = ms.at("idents");
    const value body = ms.at("body");

    // Construct (values type ...*) composed of types of `idents`
    value types = nil;
    for (const opi::value ident : range(idents))
    {
      const value type = _generate_type_and_copy_location(ident);
      types = append(types, list(type));
    }

    // Evaluate `body` with `values_type` as a target
    utl::state_saver _ {m_alist, m_targets};
    m_targets = types;
    return transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                begin
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  append_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    // Special handling for blocks
    return transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 and
  const match andmatch {list("and"), list("and", dot, "clauses")};
  append_rule(andmatch, [this](const auto &ms) {
    const value clauses = ms.at("clauses");
    // Evaluate all clauses with current target
    // TODO: add some boolean-convertable test
    const value newclauses =
        list(range(clauses) | std::views::transform(std::ref(*this)));
    // Wrap all resulting Prolog expressions in an AND
    return cons("and", newclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 or
  // NOTE: code duplication from and
  const match ormatch {list("or"), list("or", dot, "clauses")};
  append_rule(ormatch, [this](const auto &ms) {
    const value clauses = ms.at("clauses");
    const value newclauses =
        list(range(clauses) | std::views::transform(std::ref(*this)));
    return cons("and", newclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                set!
  // TODO: set current target to some dedicated "undefined" type
  const match setmatch {list("set!"), list("set!", "ident", "expr")};
  append_rule(setmatch, [this, &counter](const auto &ms, value fm) {
    const value ident = ms.at("ident");
    const value expr = ms.at("expr");

    utl::state_saver _ {m_targets};

    // Code tape for all resulting Prolog expressions
    opi::stl::vector<value> tape;

    // Make sure that `ident` is a proper lvalue
    if (not issym(ident))
    {
      throw bad_code {
          std::format("Non lvalue expression in LHS of set!: {}", ident), fm};
    }
    const value identtype = _require_symbol(ident, std::back_inserter(tape));

    // Evaluate `expr`
    const value exprresult = sym(std::format("SetProxy_{}", counter++));
    m_targets = list(exprresult, dot, "_");
    const value plexpr = (*this)(expr);
    tape.push_back(plexpr);

    // Bind `ident` with result of `expr`
    const value bindexpr = list("=", identtype, exprresult);
    tape.push_back(bindexpr);

    // Wrap everything in an AND expression
    return cons("and", list(tape));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                quote
  append_rule({list("quote"), list("quote", dot, "x")}, [this](const auto &ms) {
    opi::stl::vector<value> code;

    const value resexpr =
        list("=", m_targets,
             list(_to_type(cdr(ms.at("x")), false, std::back_inserter(code))));
    code.push_back(resexpr);

    assert(code.size() >= 1);
    if (code.size() == 1)
      return code.front();
    else
      return cons("and", list(code));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms, value fm) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");

    opi::stl::vector<value> code;
    const auto totype = [&](value atom) {
      return _to_type(atom, true, std::back_inserter(code));
    };
    const value form = list(range(cons(f, xs)) | std::views::transform(totype));
    const value plform = list("result-of", form, dot, m_targets);
    code.push_back(plform);
    copy_location(fm, plform);

    assert(code.size() >= 1);
    if (code.size() == 1)
      return code.front();
    else
      return cons("and", list(code));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atom
  append_rule({nil, "x"}, [this](const auto &ms) {
    const value x = ms.at("x");

    opi::stl::vector<value> code;
    const value expr = list("=", m_targets, list(_to_type(x, true, std::back_inserter(code))));
    code.push_back(expr);

    assert(code.size() >= 1);
    if (code.size() == 1)
      return code.front();
    else
      return cons("and", list(code));
  });
}


opi::value
opi::scheme_to_prolog::transform_block(value block)
{
  if (length(block) == 0)
    return nil;

  // Split last expression from the rest
  const value revblock = reverse(block);
  value last = car(revblock);
  value rest = reverse(cdr(revblock));

  // Evaluate the `rest`
  rest = ({
    utl::state_saver _ {m_targets};
    m_targets = "_";
    list(range(rest) | std::views::transform(std::ref(*this)));
  });

  // Evaluate `last` with the actual (current) target
  last = (*this)(last);

  // Dont forget to reverse the results and wrap them in AND
  const value result = cons("and", append(rest, list(last)));
  copy_location(block, result);
  return result;
}


bool
opi::scheme_to_prolog::find_code_type(value code, value &type) const noexcept
{
  const auto it = m_type_map.find(&*code);

  // Check if code object is present in the map
  if (it == m_type_map.end())
    return false;

  // Return associated type
  type = it->second;
  return true;
}


opi::value
opi::scheme_to_prolog::find_code_type(opi::value code) const
{
  value result = nil;
  if (find_code_type(code, result))
    return result;
  throw bad_code {
      std::format("No type associated to code object"), code};
}

void
opi::scheme_to_prolog::add_global(value ident, value type)
{
  value registered_type;
  if (assoc(ident, m_global_alist, registered_type))
  {
    if (type != registered_type)
    {
      throw std::logic_error {std::format(
          "Symbol {} is already registered: old type is {}, new type is {}",
          ident, registered_type, type)};
    }
    // Otherwize do nothing (don't duplicate entries in the a-list for
    // performance conciderations)
    return;
  }
  m_global_alist = cons(cons(ident, type), m_global_alist);
}

void
opi::scheme_to_prolog::_link_code_to_type(value code, value type)
{
  if (not m_type_map.emplace(&*code, type).second)
  {
    // Ignore clash if replacing with identical type anyways
    const value oldtype = m_type_map.at(&*code);
    if (type == oldtype)
      return;

    throw duplicate_code_objects {
        std::format("Can't link code to type (duplicate code object); would "
                    "replace '{}' with '{}'",
                    oldtype, type),
        code};
  }
}


std::string
opi::scheme_to_prolog::_type_name_for(value ident) const
{ 
  if (not issym(ident))
    throw bad_code {std::format("Not a symbol: {}", ident), ident};
  return std::format(m_type_format, std::string(sym_name(ident)));
}


opi::value
opi::scheme_to_prolog::_generate_type_and_copy_location(value ident)
{
  const value type = sym(_type_name_for(ident));
  copy_location(ident, type);
  _link_code_to_type(ident, type);
  return type;
}


[[maybe_unused]] static opi::value
_unquote_times(opi::value x, size_t n)
{
  for (size_t i = 0; i < n; ++i)
  {
    const opi::value newx = list("unquote", x);
    copy_location(x, newx);
    x = newx;
  }
  return x;
}

[[maybe_unused]] static opi::value
_quote_times(opi::value x, size_t n)
{
  for (size_t i = 0; i < n; ++i)
  {
    const opi::value newx = list("quote", x);
    copy_location(x, newx);
    x = newx;
  }
  return x;
}

[[maybe_unused]] static opi::value
_quote_unquote_times(opi::value x, size_t surroundinglevel)
{
  // 1. Not nested:
  //    -> x
  // 2. nested once
  //    -> (quote (unquote x))
  // 3. multiple nesting
  //    ->  (quote (unquote (quote (unquote x))))
  for (size_t i = 0; i < surroundinglevel; ++i)
  {
    const opi::value newx = list("quote", list("unquote", x));
    copy_location(x, newx);
    x = newx;
  }
  return x;
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_require_symbol(value ident, CodeOutput out, bool lvalue)
{
  // Check for mapped types in the a-list
  struct resolve_result { value type; value code; };
  opi::stl::vector<resolve_result> variants;
  size_t nlevelsabove = 0;
  for (const value x : range(m_alist))
  {
    // NIL is marking beginning of a nesting
    if (x == nil)
    {
      nlevelsabove += 1;
      continue;
    }
    assert(x->t == tag::pair);

    // Fish for a correct identifier
    if (car(x) != ident)
      continue;

    value type = cdr(x);
    value code = nil;

    // Instantiate template
    if (type->t == tag::pair and issym(car(type), "#template"))
    {
      if (lvalue)
        throw bad_code {std::format("Not an lvalue: {}", ident), ident};

      static size_t counter = 0; // FIXME
      const value proxy = sym(std::format("Instance_{}", counter++));
      const value functemplate = _quote_unquote_times(cdr(type), nlevelsabove);
      const value tmp = sym(std::format("Tmp_{}", counter++));
      code = list(
        list("=", tmp, functemplate),
        list("if", list("nonvar", tmp),
                   list("insert-cells", tmp, proxy),
                   False)
      );
      type = proxy;
      copy_location(cdr(x), proxy);
    }
    else
      type = _quote_unquote_times(cdr(x), nlevelsabove);

    variants.push_back({type, code});
  }

  // Also check in globals
  for (const value x : range(m_global_alist))
  {
    if (car(x) == ident)
      variants.push_back({cdr(x), nil});
  }

  switch (variants.size())
  {
    case 0:
      throw bad_code {std::format("No such symbol, {}", ident), ident};

    case 1:
      _link_code_to_type(ident, variants[0].type);
      std::ranges::copy(range(variants[0].code), out);
      return variants[0].type;

    default: {
      // Crate or-expression running over the variants
      static size_t counter = 0; // FIXME
      const value tmpvar = sym(std::format("OverloadProxy_{}", counter++));
      _link_code_to_type(ident, tmpvar);
      copy_location(ident, tmpvar);

      value clauses = nil;
      for (const resolve_result &variant : variants)
      {
        const value bind = list("=", tmpvar, variant.type);
        const value clause =
            variant.code == nil ? bind
                                : cons("and", append(variant.code, list(bind)));
        if (not has_location(variant.type))
        {
          warning("type without location: {}", variant.type);

          source_location location;
          if (get_location(ident, location))
            warning("for identifier {}", display_location(location));
          // throw bad_code {"Type without location"};
        }
        copy_location(variant.type, clause);
        clauses = cons(clause, clauses);
      }
      const value orexpr = cons("or", clauses);
      *out++ = orexpr;
      copy_location(ident, orexpr);

      // Return temporary bound in the or expression
      copy_location(ident, tmpvar);
      return tmpvar;
    }
  }
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_to_type(value atom, bool resolve_symbols, CodeOutput out)
{
  value result = nil;
  switch (atom->t)
  {
    case tag::nil: result = "nil"; break;
    case tag::num: result = "num"; break;
    case tag::str: result = "str"; break;
    case tag::boolean: result = "bool"; break;

    case tag::sym:
      if (resolve_symbols)
        return _require_symbol(atom, out);
      else
        result = "sym";
      break;

    default:
      error("unimplemented transformation for '{}'", atom);
      std::terminate();
  }

  copy_location(atom, result);
  _link_code_to_type(atom, result);
  return result;
}

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


using namespace std::placeholders;

opi::scheme_to_prolog::scheme_to_prolog(size_t &counter,
                                        type_format_string format)
: m_type_format {format},
  m_target {"_"},
  m_alist {nil},
  m_global_alist {nil},
  m_lambda_gensym {counter, "Lambda{}"}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 cases
  const match casesmatch {
      list("cases"), list("cases", "exprs", cons("patterns", "branch"), "...")};
  append_rule(casesmatch, [this, &counter](const auto &ms) {
    const value exprs = ms.at("exprs");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;

    // Save original target
    const value origtarget = m_target;

    utl::state_saver _ {m_target};

    // Translate all expressions
    opi::stl::vector<value> exprproxies;
    value plexprs = nil;
    for (const value expr : range(exprs))
    {
      const value exprproxy = sym(std::format("CaseProxy_{}", counter++));
      m_target = exprproxy;
      const value plexpr = (*this)(expr);
      plexprs = append(plexprs, list(plexpr));
      exprproxies.push_back(exprproxy);
    }

    // Translate cases
    value plcases = nil;
    for (const auto &[rowpatterns, branch] :
         utl::zip(range(patterns), range(branches)))
    {
      utl::state_saver _ {m_alist, m_target};

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

      m_target = origtarget;
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
      utl::state_saver _ {m_target};
      m_target = "boolean";
      (*this)(cond);
    });

    const value newthen = (*this)(thenbr);
    const value newelse = (*this)(elsebr);

    return list("and", newcond, newthen, newelse);
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

      // Save info for translator

      // Transform expression with target set to the identifier type and
      // accumulate resulting Prolog expression to the `result`
      // and accumulate transformed expression in results
      utl::state_saver _ {m_target};
      m_target = type;
      result = cons((*this)(expr), result);
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

      value types = nil;
      // Generate type name and add resulting identifier+type entry to the a-list
      for (const value ident : range(subidents))
      {
        const value type = _generate_type_and_copy_location(ident);
        types = append(types, list(type));
        m_alist = cons(cons(ident, type), m_alist);
      }

      // Transform expression with target set to the type-list and
      // accumulate resulting Prolog expression to the `result`
      utl::state_saver _ {m_target};
      m_target = length(types) == 1 ? car(types) : types;
      result = cons((*this)(expr), result);
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
  append_rule({list("declare-template-overload"), ovdeclpat}, [this](const auto &ms) {
    const value ovident = ms.at("ovident");
    const value ident = ms.at("ident");

    // Generate types
    const value typevar = _generate_type_and_copy_location(ident);
    m_alist = cons(cons(ovident, cons("#template", typevar)), m_alist);

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
    m_alist = cons(cons(ident, cons("#template", typevar)), m_alist);

    return list("and");
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             template
  // NOTE: leaks a-list
  const value temppat = list("template", cons("ident", "params"), dot, "body");
  append_rule({list("template"), temppat}, [this, &counter](const auto &ms, value fm) {
    const value ident = ms.at("ident");
    const value params = ms.at("params");
    const value body = ms.at("body");

    assert(sym_name(ident)[0] != '_' and not isupper(sym_name(ident)[0]) &&
           "Tempalte identifiers can't look like Prolog variables");
    const value proxyvar = _generate_type_and_copy_location(ident);
    _link_code_to_type(fm, proxyvar);

    // Will revert a-list from any changes after this point
    utl::state_saver _ {m_alist, m_target};

    // Mark beginning of the nesting by inserting a NIL value into the alist
    m_alist = cons(nil, m_alist);

    // Translate function parameters into (local) type variables
    value plparams = nil;
    for (const value x : range(params))
    {
      const value xtype = _generate_type_and_copy_location(x);
      plparams = cons(xtype, plparams);
      m_alist = cons(cons(x, xtype), m_alist);
    }
    plparams = reverse(plparams); // We were inserting at front, so now reverse

    // (local) Type variable to hold function return type
    // FIXME: why does it have to generate different identifiers?
    const value plresult = sym(std::format("Result_{}", counter++));

    // Translate the body with apropriate target
    m_target = plresult;
    const value plbody = transform_block(body);

    // Wrapp it up. Instances of this template can now be created by simple
    // calling `insert-cells` on the structure below
    // NOTE:
    // - quasiquote to prevent evaluation of all the type variables straight away
    // - variables from the higher level(s) will be captured via quasiquote(s)
    const value function_template =
        list("quasiquote", list("#dynamic-function-dispatch", ident, plparams,
                                plresult, plbody));

    return list("=", proxyvar, function_template);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "params", dot, "body")};
  append_rule(lambdamatch, [this, &counter](const auto &ms, value fm) {
    const value params = ms.at("params");
    const value body = ms.at("body");

    // Save current target
    const value target = m_target;

    // Will revert a-list from any changes after this point
    utl::state_saver _ {m_alist, m_target};

    // Mark beginning of the nesting by inserting a NIL value into the alist
    m_alist = cons(nil, m_alist);

    // Translate function parameters into (local) type variables
    value plparams = nil;
    for (const value x : range(params))
    {
      const value xtype = _generate_type_and_copy_location(x);
      plparams = cons(xtype, plparams);
      m_alist = cons(cons(x, xtype), m_alist);
    }
    plparams = reverse(plparams); // We were inserting at front, so now reverse

    // (local) Type variable to hold function return type
    // FIXME: why does it have to generate different identifiers?
    const value plresult = sym(std::format("Result_{}", counter++));


    // Translate the body with apropriate target
    m_target = plresult;
    const value plbody = transform_block(body);

    const value type_template =
        list("quasiquote", list("#dynamic-function-dispatch", "anonymous-lambda",
                                plparams, plresult, plbody));

    const value proxyvar = m_lambda_gensym();

    // TODO: this is ugly
    _link_code_to_type(fm, proxyvar);
    _link_code_to_type(car(fm), target);

    return list("and", list("=", proxyvar, type_template),
                list("insert-cells", proxyvar, target));
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
    m_alist = cons(cons(ident, type), m_alist);

    // Set new type as target for body evaluation
    utl::state_saver _ {m_alist, m_target};
    m_target = type;
    return transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                begin
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  append_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    // Special handling for blocks
    const value newbody = transform_block(body);
    return cons("begin", newbody);
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
    return cons("or", newclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                quote
  append_rule({list("quote"), list("quote", dot, "x")}, [this](const auto &ms) {
    std::vector<value> code;

    const value resexpr =
        list("=", m_target,
             _to_type(cdr(ms.at("x")), false, std::back_inserter(code)));
    code.push_back(resexpr);

    assert(code.size() >= 1);
    if (code.size() == 1)
      return code.front();
    else
      return cons("and", list(code));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");

    std::vector<value> code;
    const auto totype = [&](value atom) {
      return _to_type(atom, true, std::back_inserter(code));
    };
    const value form = list(range(cons(f, xs)) | std::views::transform(totype));
    code.push_back(list("result-of", form, m_target));

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

    std::vector<value> code;
    const value expr = list("=", m_target, _to_type(x, true, std::back_inserter(code)));
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
    utl::state_saver _ {m_target};
    m_target = "_";
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


static opi::value
_unquote_times(opi::value x, size_t n)
{
  for (size_t i = 0; i < n; ++i)
    x = list("unquote", x);
  return x;
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_require_symbol(value ident, CodeOutput out)
{
  // Check for mapped types in the a-list
  std::vector<value> variants;
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

    // Instantiate template
    if (type->t == tag::pair and issym(car(type), "#template"))
    {
      const value functemplate = _unquote_times(cdr(type), nlevelsabove);
      static size_t counter = 0; // FIXME
      const value proxy = sym(std::format("Instance_{}", counter++));
      copy_location(ident, proxy);
      *out++ = list("insert-cells", functemplate, proxy);
      type = proxy;
    }
    else
      type = _unquote_times(cdr(x), nlevelsabove);

    variants.push_back(type);
  }

  // Also check in globals
  for (const value x : range(m_global_alist))
  {
    if (car(x) == ident)
      variants.push_back(cdr(x));
  }

  switch (variants.size())
  {
    case 0:
      throw bad_code {std::format("No such symbol, {}", ident), ident};

    case 1:
      _link_code_to_type(ident, variants[0]);
      return variants[0];

    default: {
      // Crate or-expression running over the variants
      static size_t counter = 0; // FIXME
      const value tmpvar = sym(std::format("OverloadProxy_{}", counter++));
      _link_code_to_type(ident, tmpvar);
      copy_location(ident, tmpvar);

      value clauses = nil;
      for (const value variant : variants)
        clauses = cons(list("=", tmpvar, variant), clauses);
      *out++ = cons("or", clauses);

      // Return temporary bound in the or expression
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
    case tag::boolean: result = "boolean"; break;

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

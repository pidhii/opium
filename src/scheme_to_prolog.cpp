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
#include "opium/stl/vector.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/logging.hpp"
#include "opium/value.hpp"
#include "opium/utilities/ranges.hpp"
#include "opium/lisp_parser.hpp"
#include <iterator>


using namespace std::placeholders;


opi::value
opi::prolog_emitter::default_literal_type_coder(value literal)
{
  switch (tag(literal))
  {
    case tag::num:
      return "num";
    case tag::str:
      return "str";
    case tag::boolean:
      return "bool";
    default:
      throw bad_code {"default_literal_type_coder - unsupported literal",
                      literal};
  }
}


opi::value
opi::prolog_emitter::transformer::transform_block(opi::value block) const
{
  throw std::logic_error {"Not supposed to use block-transformation of "
                          "prolog_emitter::transformer"};
}

void
opi::prolog_emitter::setup_prolog(prolog &pl)
{
  // Helpers for type coercions
  pl.add_predicate("(coerce-list In Out)"_lisp,
                   "(if (= In Out) #t (fast-coerce-list In Out))"_lisp);

  // pl.add_predicate("(fast-coerce-list (In) (Out))"_lisp, "(fast-coerce In Out)"_lisp);
  pl.add_predicate("(fast-coerce-list () ())"_lisp, True);
  pl.add_predicate(
      "(fast-coerce-list (InHead . InTail) (OutHead . OutTail))"_lisp,
      "(and (fast-coerce InHead OutHead) (fast-coerce-list InTail OutTail))"_lisp);

  pl.add_predicate("(fast-coerce In Out)"_lisp,
                   "(if (= In Out) #t (distinct (coerce In Out)))"_lisp);

  // Type-check rule for dynamic function specialization
  pl.add_predicate(
      "(result-of ((#dynamic-function-dispatch _ Args Results Body) . ArgsIn) . Results)"_lisp,
      // inline of `coerce-list` from above
      "(and (if (= ArgsIn Args) #t                     \
                (fast-coerce-list ArgsIn Args))        \
            (call Body))"_lisp);
}


opi::prolog_emitter::prolog_emitter()
: m_targets {"_"},
  m_alist {nil},
  m_global_alist {nil},
  m_typecoder {default_literal_type_coder},
  m_typevargen {m_counter, "Opi#typevar{}"},
  m_termgen {m_counter, "opi#term{}"}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                            inline-prolog
  const match inlplmatch {list("pragma", "inline-prolog"),
                          list("pragma", "inline-prolog", dot, "statements")};
  m_T.append_rule(inlplmatch, [](const auto &ms) {
    return cons("and", ms.at("statements"));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                            other pragmas (omit)
  const match pragmatch {list("pragma"), list("pragma", dot, "_")};
  m_T.append_rule(pragmatch, [](const auto &) { return True; });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             annotate-type
  m_T.append_rule(
    {
      "(annotate-type)"_lisp,
      "(annotate-type expr . types)"_lisp
    },
    [this](const auto &ms) {
    const value expr = ms.at("expr");
    const value types = ms.at("types");

    utl::state_saver _ {m_targets};

    // Evaluate `expr` with expliciit targets
    if (m_targets == "_")
    {
      // Slight optimization for type-ignorant surrounding context
      m_targets = types;
      return m_T(expr);
    }
    else
      return list("and", list("=", m_targets, types), m_T(expr));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               coerce
  m_T.append_rule(
    {
      "(coerce)"_lisp,
      "(coerce expr . types)"_lisp
    },
    [this](const auto &ms) {
    const value expr = ms.at("expr");
    const value types = ms.at("types");

    // Coerce natural results of `expr` towards the specified `types`
    const value exprtgts = m_typevargen();
    const value newexpr = ({
      utl::state_saver _ {m_targets};
      m_targets = exprtgts;
      m_T(expr);
    });
    const value coercion = list("coerce-list", exprtgts, types);
    const value bindtarget = list("=", m_targets, types);
    return list("and", newexpr, coercion, bindtarget);
  });


  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 cases
  const match casesmatch {
      list("cases"), list("cases", "idents", cons("patterns", "branch"), "...")};
  m_T.append_rule(casesmatch, [this](const auto &ms) {
    const value idents = ms.at("idents");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;

    // Save original target
    const value origtargets = m_targets;

    utl::state_saver _ {m_targets};
    stl::vector<value> tape;

    // Translate all expressions
    opi::stl::vector<value> identtypes;
    // value plexprs = nil;
    for (const value ident : range(idents))
    {
      assert(issym(ident));
      const value type = _require_symbol(ident, std::back_inserter(tape));
      identtypes.push_back(type);
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
           utl::zip(range(rowpatterns), identtypes))
      {
        if (ispair(pattern))
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
      const value plbranch = _transform_block(branch);

      // Add all matches and the branch to plcases
      plcases = append(plcases, plmatches);
      plcases = append(plcases, list(plbranch));
    }

    // warning("cases in: {}", fm);
    // warning("cases out: {}", plcases);

    // Combine all expressions and cases
    return cons("and", append(list(tape), plcases));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 if
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  m_T.append_rule(ifmatch, [this](const auto &ms) {
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");
    const value elsebr = ms.at("else");

    // <cond> must evaluate into boolean
    const value newcond = ({
      utl::state_saver _ {m_targets};
      m_targets = list("bool", dot, "_");
      m_T(cond);
    });

    const value thentgts = m_typevargen();
    const value elsetgts = m_typevargen();

    if (m_targets == "_")
    {
      const value newthen = m_T(thenbr);
      const value newelse = m_T(elsebr);
      return list("and", newcond, newthen, newelse);
    }
    else
    {
      const value newthen = ({
        utl::state_saver _ {m_targets};
        m_targets = thentgts;
        m_T(thenbr);
      });

      const value newelse = ({
        utl::state_saver _ {m_targets};
        m_targets = elsetgts;
        m_T(elsebr);
      });

      const value coercebranches =
          list("if", list("and", list("coerce-list", thentgts, elsetgts),
                                 list("=", m_targets, elsetgts)),
                     True,
                     list("and", list("coerce-list", elsetgts, thentgts),
                                 list("=", m_targets, thentgts)));

      return list("and", newcond, newthen, newelse, coercebranches);
    }
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 if (no else-branch)
  const match ifnoelsematch {list("if"), list("if", "cond", "then")};
  m_T.append_rule(ifnoelsematch, [this](const auto &ms, value fm) {
    // TODO: remove this '?'-related stuff from here (revert the changes), and
    //       it to a separate plugin
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");

    // Since there is no <else> branch this expression is only valid with a
    // wildcard target (NOT ANYMORE)
    if (m_targets != "_")
    {
      source_location location;
      get_location(fm, location) or get_location(cond, location) or
          get_location(thenbr, location);
      throw bad_code {
          std::format(
              "Can't evaluate else-less if-expression with exact target ({})",
              m_targets),
          fm};
    }

    // <cond> must evaluate into boolean
    const value newcond = ({
      utl::state_saver _ {m_targets};
      m_targets = list("bool", dot, "_");
      m_T(cond);
    });

    const value newthen = m_T(thenbr);
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
    for (; ispair(idents); idents = cdr(idents), exprs = cdr(exprs))
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
      result = append(result, list(m_T(expr)));
    }

    // Process body and accumulate in results
    result = cons(_transform_block(ms.at("body")), result);

    // Reverse results as we were accumulating from the front
    return cons("and", reverse(result));
  };

  // Add rules
  const value letpat = list(list(list("ident", "expr"), "..."), dot, "body");
  m_T.append_rule({list("let"), cons("let", letpat)}, letrule);
  m_T.append_rule({list("let*"), cons("let*", letpat)}, letrule);
  m_T.append_rule({list("letrec"), cons("letrec", letpat)}, letrule);
  m_T.append_rule({list("letrec*"), cons("letrec*", letpat)}, letrule);

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
    for (; ispair(idents); idents = cdr(idents), exprs = cdr(exprs))
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
      result = append(result, list(m_T(expr)));
    }

    // Process body and accumulate in results
    result = cons(_transform_block(ms.at("body")), result);

    // Reverse results as we were accumulating from the front
    return cons("and", reverse(result));
  };

  // Add rules
  m_T.append_rule({list("let-values"), cons("let-values", letpat)}, letvalsrule);
  m_T.append_rule({list("let*-values"), cons("let*-values", letpat)}, letvalsrule);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                        declare-template-overload
  // NOTE: leaks a-list
  const value ovdeclpat = list("declare-template-overload", "ovident", "ident");
  m_T.append_rule({list("declare-template-overload"), ovdeclpat},
                        [this](const auto &ms, [[maybe_unused]]value fm) {
    const value ovident = ms.at("ovident");
    const value ident = ms.at("ident");

    // Generate types
    const value typevar = _generate_type_and_copy_location(ident);
    const value type = cons("#template", typevar);
    m_alist = cons(cons(ovident, type), m_alist);
    copy_location(ident, type);

    return True;
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                        declare-template
  // NOTE: leaks a-list
  const value declpat = list("declare-template", "ident");
  m_T.append_rule({list("declare-template"), declpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");

    // Generate types
    const value typevar = _generate_type_and_copy_location(ident);
    const value type = cons("#template", typevar);
    m_alist = cons(cons(ident, type), m_alist);
    copy_location(ident, type);

    return True;
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             declare
  // NOTE: leaks a-list
  const value declonepat = list("declare", "ident");
  m_T.append_rule({list("declare"), declonepat}, [this](const auto &ms) {
    const value ident = ms.at("ident");

    // Generate types
    const value type = _generate_type_and_copy_location(ident);
    m_alist = cons(cons(ident, type), m_alist);
    copy_location(ident, type);

    return True;
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             template
  // NOTE: leaks a-list
  const value temppat = list("template", cons("ident", "params"), dot, "body");
  m_T.append_rule({list("template"), temppat}, [this](const auto &ms, value fm) {
    value ident = ms.at("ident");
    const value params = ms.at("params");
    const value body = ms.at("body");

    value plresults;
    if (ispair(ident))
    {
      // Syntax with explicit return type declaration
      plresults = cdr(ident);
      ident = car(ident);
    }
    else
      // No explicit return type declaration
      plresults = sym("Results");

    const value proxyvar = _generate_type_and_copy_location(ident);
    m_ctm->assign_type(fm, proxyvar);

    // Will revert a-list from any changes after this point.
    utl::state_saver _ {m_alist, m_targets};

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
      plparams = append_mut(plparams, list(xtype));
      m_alist = cons(cons(x, xtype), m_alist);
    }


    // Translate the body with apropriate target
    m_targets = plresults;
    // const value plbody = clean_prolog(_transform_block(body)); // FIXME
    const value plbody = _transform_block(body);

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
  m_T.append_rule(lambdamatch, [this](const auto &ms, value fm) {
    const value params = ms.at("params");
    const value body = ms.at("body");

    // Save current target
    const value targets = m_targets;

    // Will revert a-list from any changes after this point
    utl::state_saver _ {m_alist, m_targets};

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
    const value plresults = sym("Results");

    // Translate the body with apropriate target
    m_targets = plresults;
    const value plbody = clean_prolog(_transform_block(body)); // NOTE: may cause segfault
    // const value plbody = _transform_block(body);

    // Generate unique identifier for this lambda
    const value lambdaname = m_termgen();
    const value functemplate =
        list("quasiquote", list("#dynamic-function-dispatch", lambdaname,
                                plparams, plresults, plbody));

    const value proxyvar = m_typevargen();
    const value instvar = m_typevargen();

    // TODO: this is ugly
    m_ctm->assign_type(fm, proxyvar);
    m_ctm->assign_type(car(fm), instvar); // FIXME

    return list("and", list("=", proxyvar, functemplate),
                       list("insert-cells", proxyvar, instvar),
                       list("=", targets, list(instvar)));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  // NOTE: leaks a-list
  const value defpat = list("define", "ident", dot, "body");
  m_T.append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.at("body");

    // Add identifier to the alist
    const value type = _generate_type_and_copy_location(ident);

    // Set new type as target for body evaluation
    utl::state_saver _ {m_alist, m_targets};
    m_targets = list(type, dot, "_");
    return _transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                            define-values
  // NOTE: leaks a-list
  const value defvalspat = list("define-values", "idents", dot, "body");
  m_T.append_rule({list("define-values"), defvalspat}, [this](const auto &ms) {
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
    return _transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                begin
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  m_T.append_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    // Special handling for blocks
    return _transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 and
  const match andmatch {list("and"), list("and", dot, "clauses")};
  m_T.append_rule(andmatch, [this](const auto &ms) {
    const value clauses = ms.at("clauses");
    // Evaluate all clauses with current target
    // TODO: add some boolean-convertable test

    const value newclauses =
        list(range(clauses) | std::views::transform(std::ref(m_T)));
    // Wrap all resulting Prolog expressions in an AND
    return cons("and", newclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 or
  // NOTE: code duplication from and
  const match ormatch {list("or"), list("or", dot, "clauses")};
  m_T.append_rule(ormatch, [this](const auto &ms) {
    const value clauses = ms.at("clauses");
    const value newclauses =
        list(range(clauses) | std::views::transform(std::ref(m_T)));
    return cons("and", newclauses);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                set!
  // TODO: set current target to some dedicated "undefined" type
  const match setmatch {list("set!"), list("set!", "ident", "expr")};
  m_T.append_rule(setmatch, [this](const auto &ms, value fm) {
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
    const value exprresult = m_typevargen();
    m_targets = list(exprresult, dot, "_");
    const value plexpr = m_T(expr);
    tape.push_back(plexpr);

    // Bind `ident` with result of `expr`
    const value bindexpr = list("fast-coerce", exprresult, identtype);
    tape.push_back(bindexpr);

    // Wrap everything in an AND expression
    return cons("and", list(tape));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                quote
  m_T.append_rule({list("quote"), list("quote", dot, "x")}, [this](const auto &ms) {
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
  //                           IMPORT PLUGINS
  scheme_syntax_plugin::apply_all(*this);

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  m_T.append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms, value fm) {
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
  m_T.append_rule({nil, "x"}, [this](const auto &ms) {
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
opi::prolog_emitter::_transform_block(value block)
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
    list(range(rest) | std::views::transform(std::ref(m_T)));
  });

  // Evaluate `last` with the actual (current) target
  last = m_T(last);

  // Dont forget to reverse the results and wrap them in AND
  const value result = cons("and", append(rest, list(last)));
  copy_location(block, result);
  return result;
}


void
opi::prolog_emitter::add_global(value ident, value type)
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


std::string
opi::prolog_emitter::_type_name_for(value ident) const
{ 
  if (not issym(ident))
    throw bad_code {std::format("Not a symbol: {}", ident), ident};
  return std::format("T:{}", sym_name(ident));
}


opi::value
opi::prolog_emitter::_generate_type_and_copy_location(value ident)
{
  const value type = sym(_type_name_for(ident));
  copy_location(ident, type);
  m_ctm->assign_type(ident, type);
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

static opi::value
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
opi::prolog_emitter::_require_symbol(value ident, CodeOutput out, bool lvalue)
{
  // Check for mapped types in the a-list
  struct resolve_result { value type; value code; };
  opi::stl::vector<resolve_result> variants;
  size_t nlevelsabove = 0;
  for (const value x : range(m_alist))
  {
    // NIL marks the beginning of a scope
    if (x == nil)
    {
      nlevelsabove += 1;
      continue;
    }
    assert(ispair(x));

    // Fish for a correct identifier
    if (car(x) != ident)
      continue;

    value type = cdr(x);
    value code = nil;

    // Instantiate template
    if (ispair(type) and issym(car(type), "#template"))
    {
      if (lvalue)
        throw bad_code {std::format("Not an lvalue: {}", ident), ident};

      const value proxy = m_typevargen();
      const value functemplate = _quote_unquote_times(cdr(type), nlevelsabove);
      const value tmp = m_typevargen();
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
      // throw bad_code {std::format("No such symbol, {}", ident), ident};
      m_ctm->assign_type(ident, ident);
      return ident;

    case 1:
      m_ctm->assign_type(ident, variants[0].type);
      std::ranges::copy(range(variants[0].code), out);
      return variants[0].type;

    default: {
      // Crate or-expression running over the variants
      const value tmpvar = m_typevargen();
      m_ctm->assign_type(ident, tmpvar);
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
opi::prolog_emitter::_to_type(value atom, bool resolve_symbols, CodeOutput out)
{
  value result = nil;
  if (issym(atom) and resolve_symbols)
    return _require_symbol(atom, out);

  result = m_typecoder(atom);
  copy_location(atom, result);
  m_ctm->assign_type(atom, result);
  return result;
}

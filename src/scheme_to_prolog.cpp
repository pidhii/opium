#include "opium/lisp_parser.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/utilities/state_saver.hpp"
#include "opium/logging.hpp"


using namespace std::placeholders;

opi::scheme_to_prolog::scheme_to_prolog(size_t &counter,
                                        type_format_string format)
: m_type_format {format},
  m_target {"_"},
  m_alist {nil},
  m_global_alist {nil},
  m_unresolved {nil},
  m_lambda_gensym {counter, "lambda{}"}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                 if
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  append_rule(ifmatch, [this, &counter](const auto &ms) {
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");
    const value elsebr = ms.at("else");


    // <cond> must evaluate into boolean
    const value newcond = ({
      utl::state_saver _ {m_target};
      m_target = "boolean";
      (*this)(cond);
    });

    // Evaluate <then> and <else> with separate proxy targets
    const value thentarget = symbol_generator {counter, "Then{}"}();
    const value elsetarget = symbol_generator {counter, "Else{}"}();
    const value newthen = ({
      utl::state_saver _ {m_target};
      m_target = thentarget;
      (*this)(thenbr);
    });
    const value newelse = ({
      utl::state_saver _ {m_target};
      m_target = elsetarget;
      (*this)(elsebr);
    });

    // Result is a union of the <then> and <else> results
    return list("and", newcond, newthen, newelse,
                list("unify", thentarget, elsetarget, m_target));
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
      const value type = sym(_type_name_for(ident));
      copy_location(ident, type); // TODO do this location propagation automatic (reimpelemnt _type_name_for)
      m_alist = cons(cons(ident, type), m_alist);

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
        const value type = sym(_type_name_for(ident));
      copy_location(ident, type);
        types = append(types, list(type));
        m_alist = cons(cons(ident, type), m_alist);
      }

      // Transform expression with target set to the type-list and
      // accumulate resulting Prolog expression to the `result`
      utl::state_saver _ {m_target};
      m_target = types;
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
  //                        define (function syntax)
  // NOTE: leaks a-list
  const value deffnpat = list("define", list("ident", dot, "parms"), dot, "body");
  append_rule({list("define"), deffnpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value params = ms.at("parms");
    const value body = ms.at("body");

    // Forward declaration of the identifier
    const value type = sym(_type_name_for(ident));
    copy_location(ident, type);
    m_alist = cons(cons(ident, type), m_alist);

    // Build lambda
    const value lambdatype = _lambda(std::format("lambda:{}", ident), params, body);

    // Identifiy identifier with lambda
    return list("=", type, lambdatype);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               define
  // NOTE: leaks a-list
  const value defpat = list("define", "ident", dot, "body");
  append_rule({list("define"), defpat}, [this](const auto &ms) {
    const value ident = ms.at("ident");
    const value body = ms.at("body");

    const value type = sym(_type_name_for(ident));
    copy_location(ident, type);

    // Add identifier to the alist and set as target for body evaluation
    m_alist = cons(cons(ident, type), m_alist);
    utl::state_saver _ {m_alist, m_target};
    m_target = type;
    return transform_block(body);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "parms", dot, "body")};
  append_rule(lambdamatch, [this](const auto &ms) {
    const value parms = ms.at("parms");
    const value body = ms.at("body");
    const value type = _lambda(sym_name(m_lambda_gensym()), parms, body);
    return list("=", m_target, type);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                begin
  const match beginmatch {list("begin"), list("begin", dot, "body")};
  append_rule(beginmatch, [this](const auto &ms) {
    const value body = ms.at("body");
    const value newbody = transform_block(body);
    return cons("begin", newbody);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                quote
  append_rule({list("quote"), list("quote", dot, "x")}, [this](const auto &ms) {
    return list("=", m_target, _to_type(cdr(ms.at("x"))));
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                form
  append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const auto totype = std::bind(&scheme_to_prolog::_to_type, this, _1);
    const value form = list(range(cons(f, xs)) | std::views::transform(totype));
    return list("result-of*", form, m_target);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                atom
  append_rule({nil, "x"}, [this](const auto &ms) {
    const value x = ms.at("x");
    return list("=", m_target, _to_type(x));
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


std::string
opi::scheme_to_prolog::_type_name_for(value ident) const
{ return std::format(m_type_format, std::string(sym_name(ident))); }


opi::value
opi::scheme_to_prolog::_require_symbol(value ident)
{
  // Get mapped type from the a-list if its there
  value result = nil;
  if (assoc(ident, m_alist, result))
    return result;

  // Otherwize, check it in a-list with globals
  if (assoc(ident, m_global_alist, result))
    return result;

  // If not found, create new associated type for this symbol
  const value type = sym(_type_name_for(ident));
  copy_location(ident, type);
  m_alist = cons(cons(ident, type), m_alist);

  // Add it to the list of unresolved symbols that will be handled by closure
  m_unresolved = cons(cons(ident, type), m_unresolved);

  // Return the new associated type
  return type;
}


opi::value
opi::scheme_to_prolog::_to_type(value atom)
{
  value result = nil;
  switch (atom->t)
  {
    case tag::nil: result = "nil"; break;
    case tag::num: result = "num"; break;
    case tag::str: result = "str"; break;
    case tag::boolean: result = "boolean"; break;

    case tag::sym:
      return _require_symbol(atom);

    default:
      error("unimplemented transformation for '{}'", atom);
      std::terminate();
  }

  copy_location(atom, result);
  return result;
}

opi::value
opi::scheme_to_prolog::_lambda(std::string_view name, value params, value body)
{
  value plparms = nil;
  value predbody = nil;
  value unresolved = nil;
  {
    // Reset a-list, target, and unresolved symbols for new closure
    utl::state_saver _ {m_alist, m_unresolved, m_target};
    m_alist = nil;
    m_unresolved = nil;
    m_target = "Result";

    // Process function parameters adding them to a-list and saving list of
    // associated types for further cration of predicate
    for (const value x : range(params))
    {
      const value xtype = sym(_type_name_for(x));
      copy_location(x, xtype);
      plparms = cons(xtype, plparms);
      m_alist = cons(cons(x, xtype), m_alist);
    }
    // Reverese `plparms` because we were accumulating them from the front
    plparms = reverse(plparms);

    // Evaluate function body
    predbody = transform_block(body);

    // Save unresolved symbols (before before undone by `state_saver`) for
    // further resolution in surrounding context
    unresolved = m_unresolved;
  }

  if (length(unresolved) == 0) // Case 1: empty colosure
  {
    // Use function name (must be unique) as type identifier
    const value type = sym(name);

    // Create predicate for type-deduction on this function
    const value funcsig = list(type, dot, plparms);
    const value predsig = list("result-of", funcsig, "Result");
    m_predicates.emplace_back(predsig, predbody);

    return type;
  }
  else // Case 2: non-empty closure
  {
    const value closure_idents =
        list(range(unresolved) | std::views::transform(car<true>));
    const value closure_types =
        list(range(unresolved) | std::views::transform(cdr<true>));

    // Resolve closure symbols
    // Note: will also trigger propagation from higher closures if necessary
    auto require_symbol =
        std::bind(&scheme_to_prolog::_require_symbol, this, _1);
    const value resolved_closure =
        list(range(closure_idents) | std::views::transform(require_symbol));

    // Create predicate for type-deduction on this function
    // Note: signature with implicit closure bindings
    const value funcsig = list(cons(sym(name), closure_types), dot, plparms);
    const value predsig = list("result-of", funcsig, "Result");
    m_predicates.emplace_back(predsig, predbody);

    return cons(sym(name), resolved_closure);
  }
}
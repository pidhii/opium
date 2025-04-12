#include "opium/code_transformer.hpp"
#include "opium/source_location.hpp"
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
  //                               template
  // NOTE: leaks a-list
  const value temppat = list("template", cons("ident", "params"), dot, "body");
  append_rule({list("template"), temppat}, [this, &counter](const auto &ms) {
    const value ident = ms.at("ident");
    const value params = ms.at("params");
    const value body = ms.at("body");

    // TODO: make safe name generation
    const std::string templatename =
        std::format("template:{}_{}", ident, counter++);

    // Forward declaration of the identifier
    const value typetemplate =
        list("#template", sym(templatename), nil, length(params));
    m_global_alist = cons(cons(ident, typetemplate), m_global_alist);

    // Build lambda associating it with a new unique type
    const value lambdatype = _create_template(templatename, params, body);
    assert(lambdatype == typetemplate);

    // Link identifier (as a code object) with the unique type generated above
    _link_code_to_overload_name(ident, sym(templatename));

    return list("and");
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
  //                               lambda
  const match lambdamatch {list("lambda"), list("lambda", "params", dot, "body")};
  append_rule(lambdamatch, [this](const auto &ms, value fm) {
    const value params = ms.at("params");
    const value body = ms.at("body");

    const value name = m_lambda_gensym();

    opi::stl::vector<value> code;
    const value typetemplate = _create_template(sym_name(name), params, body);
    const value instance =
        _instantiate_template(typetemplate, std::back_inserter(code));
    code.push_back(list("=", m_target, instance));

    _link_code_to_overload_name(fm, name);
    _link_code_to_type(fm, m_target);

    assert(code.size() >= 1);
    if (code.size() == 1)
      return code.front();
    else
      return cons("and", list(code));
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


bool
opi::scheme_to_prolog::find_overload_name(value code,
                                          value &name) const noexcept
{
  const auto it = m_overloads.find(&*code);

  // Check if code object is present in the map
  if (it == m_overloads.end())
    return false;

  // Return associated name
  name = it->second;
  return true;
}


opi::value
opi::scheme_to_prolog::find_overload_name(opi::value code) const
{
  value result = nil;
  if (find_overload_name(code, result))
    return result;
  throw bad_code {
      std::format("No overload name associated to code object"), code};
}


bool
opi::scheme_to_prolog::is_overload_name(value name) const noexcept
{
  for (const auto &[_, overloadname] : m_overloads)
  {
    if (name == overloadname)
      return true;
  }
  return false;
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


void
opi::scheme_to_prolog::_link_code_to_overload_name(value code, value name)
{
  if (not m_overloads.emplace(&*code, name).second)
    throw duplicate_code_objects {"Can't link overload (duplicate code object)",
                                  code};
}


std::string
opi::scheme_to_prolog::_type_name_for(value ident) const
{ return std::format(m_type_format, std::string(sym_name(ident))); }


opi::value
opi::scheme_to_prolog::_generate_type_and_copy_location(value ident)
{
  const value type = sym(_type_name_for(ident));
  copy_location(ident, type);
  _link_code_to_type(ident, type);
  return type;
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_require_symbol(value ident, CodeOutput out)
{
  // Check for mapped types in the a-list
  std::vector<value> variants;
  const auto filter = [&](value x) { return car(x) == ident; };
  std::ranges::copy(range(m_alist)
                    | std::views::filter(filter)
                    | std::views::transform(cdr<false>),
                    std::back_inserter(variants));
  std::ranges::copy(range(m_global_alist)
                    | std::views::filter(filter)
                    | std::views::transform(cdr<false>),
                    std::back_inserter(variants));

  // Instantiate templates
  for (value &variant : variants)
  {
    if (variant->t == tag::pair and issym(car(variant), "#template"))
    {
      static size_t counter = 0; // FIXME
      const value proxy = sym(std::format("TemplateInstantiation_{}", counter++));
      copy_location(ident, proxy);
      const value instantiation = _instantiate_template(variant, out);
      *out++ = list("=", proxy, instantiation);
      variant = proxy;
    }
  }

  switch (variants.size())
  {
    case 0: {
      // If not found, create new associated type for this symbol
      const value type = _generate_type_and_copy_location(ident);
      m_alist = cons(cons(ident, type), m_alist);

      // Add it to the list of unresolved symbols that will be handled by closure
      // NOTE: in a way we are generating code here, thus must copy symbols to avoid
      // clashes of code objects
      m_unresolved = cons(cons(sym(sym_name(ident)), type), m_unresolved);

      // Return the new associated type
      return type;
    }

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


opi::value
opi::scheme_to_prolog::_create_template(std::string_view name, value params,
                                        value body)
{
  value plparams = nil;
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
      const value xtype = _generate_type_and_copy_location(x);
      plparams = cons(xtype, plparams);
      m_alist = cons(cons(x, xtype), m_alist);
    }
    // Reverese `plparams` because we were accumulating them from the front
    plparams = reverse(plparams);

    // Evaluate function body
    predbody = transform_block(body);

    // Save unresolved symbols (before before undone by `state_saver`) for
    // further resolution in surrounding context
    unresolved = m_unresolved;
  }

  const value closure_idents =
      list(range(unresolved) | std::views::transform(car<true>));
  const value closure_types =
      list(range(unresolved) | std::views::transform(cdr<true>));

  // Create predicate for type-deduction on this function
  // Note: signature with implicit closure bindings
  const value typeparams = append(closure_types, plparams);
  const value funcsig = list(cons(sym(name), typeparams), dot, plparams);
  const value predsig = list("result-of", funcsig, "Result");
  m_predicates.emplace_back(predsig, predbody);

  // Create type template
  return list("#template", sym(name), closure_idents, length(params));
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_instantiate_template(value ident, value closure,
                                             size_t arity, CodeOutput out)
{
  // Resolve closure symbols
  // NOTE: will also trigger propagation from higher closures if necessary
  auto require_symbol = [&](value ident) {
    return _require_symbol(ident, out);
  };
  const value resolved_closure =
      list(range(closure) | std::views::transform(require_symbol));

  value parmanchors = nil;
  static size_t counter = 0; // FIXME
  for (size_t i = 0; i < arity; ++i)
    parmanchors =
        cons(sym(std::format("T_{}_Parm{}_Uid{}", ident, i, counter++)),
             parmanchors);

  const value typeparams = append(resolved_closure, parmanchors);
  return cons(ident, typeparams);
}


template <std::output_iterator<opi::value> CodeOutput>
opi::value
opi::scheme_to_prolog::_instantiate_template(value type_template, CodeOutput out)
{
  type_template = cdr(type_template);
  const value ident = car(type_template);
  const value closure = car(cdr(type_template));
  const size_t arity = num_val(car(cdr(cdr(type_template))));
  return _instantiate_template(ident, closure, arity, out);
}

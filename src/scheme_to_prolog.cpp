#include "opium/scheme/scheme_transformations.hpp"
#include "opium/utilities/state_saver.hpp"


opi::scheme_to_prolog::scheme_to_prolog(type_format_string format)
: m_type_format {format},
  m_target {"_"},
  m_alist {nil}
{
  const match ifmatch {list("if"), list("if", "cond", "then", "else")};
  append_rule(ifmatch, [this](const auto &ms) {
    const value cond = ms.at("cond");
    const value thenbr = ms.at("then");
    const value elsebr = ms.at("else");

    return list("and", ({
                  // Evaluate <cond> with boolean target
                  utl::state_saver _ {m_target};
                  m_target = "boolean";
                  (*this)(cond);
                }),
                list("or", (*this)(thenbr), (*this)(elsebr)));
  });

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
      const value type =
          sym(std::format(m_type_format, std::string(sym_name(ident))));
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

  // Common fragment of pattern for the let-family
  const value letpat = list(list(list("ident", "expr"), "..."), dot, "body");

  // Add rules for each let-family variant
  append_rule({list("let"), cons("let", letpat)}, letrule);
  append_rule({list("let*"), cons("let*", letpat)}, letrule);
  append_rule({list("letrec"), cons("letrec", letpat)}, letrule);
  append_rule({list("letrec*"), cons("letrec*", letpat)}, letrule);
  append_rule({list("let-values"), cons("let-values", letpat)}, letrule);
  append_rule({list("let*-values"), cons("let*-values", letpat)}, letrule);

  // Form
  append_rule({nil, list("f", dot, "xs")}, [this](const auto &ms) {
    using namespace std::placeholders;
    const value f = ms.at("f");
    const value xs = ms.at("xs");
    const auto totype =
        std::bind(&scheme_to_prolog::_to_type, this, std::placeholders::_1);
    const value form = list(range(cons(f, xs)) | std::views::transform(totype));
    return list("result-of", form, m_target);
  });

  // Atoms
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

  // Reverse the block to split last expression from the rest
  const value revblock = reverse(block);
  value last = car(revblock);
  value rest = cdr(revblock);

  // Evaluate `last` with the current target
  last = (*this)(last);

  // Evaluate everything else with dummy target
  utl::state_saver _ {m_target};
  m_target = "_";
  rest = list(range(rest) | std::views::transform(std::ref(*this)));

  // Dont forget to reverse the results and wrap them in AND
  return cons("and", reverse(cons(last, rest)));
}

opi::value
opi::scheme_to_prolog::_to_type(value ident) const
{
  switch (ident->t)
  {
    case tag::nil: return "nil";
    case tag::num: return "num";

    case tag::sym: {
      value xtype = nil;
      if (assoc(ident, m_alist, xtype))
        return xtype;
      else
        return sym(std::format("builtin_{}", ident)); // FIXME
    }

    default:
      throw std::runtime_error {"unimplemented"};
  }
}
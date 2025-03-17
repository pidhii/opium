#include "opium/prolog_repl.hpp"
#include "opium/value.hpp"


void
opi::prolog_repl::operator << (opi::value expr)
{
  // New predicate
  if (issym(car(expr), "predicate"))
  {
    const value signature = car(cdr(expr));
    value body = cdr(cdr(expr));
    switch (length(body))
    {
      case 0:
        body = True;
        break;
      case 1:
        body = car(body);
        break;
      default:
        body = cons(sym("and"), body);
        break;
    }
    add_predicate(signature, body);
    return;
  }

  // Query
  if (issym(car(expr), "query"))
  {
    _query(car(cdr(expr)));
    return;
  }

  throw error {format("Don't understand expression: ", expr)};
}


void
opi::prolog_repl::_query(opi::value expr)
{
  // Run query over `expr`
  bool success = false;
  predicate_runtime prt;
  std::cout << "?- " << expr << std::endl;
  make_true(prt, expr, [&](const predicate_runtime &result_prt) {
    success = true;
    std::cout << "=> yes" << std::endl;
    for (const value var : result_prt.variables())
    {
      value val = nil;
      if (result_prt.get_value(var, val))
        std::cout << " " << var << " = " << val << std::endl;
      else
        std::cout << " " << var << " = ?" << std::endl;
    }
  });

  // Handle failed query
  if (not success)
    std::cout << "=> no" << std::endl;
}

#include "opium/prolog.inl"


bool
opi::prolog_impl::var(value x)
{
  value _ = nil;
  if (x->t == tag::pair and issym(car(x), "__cell"))
    return not get_value(static_cast<cell*>(cdr(x)->ptr), _);
  return false;
}

void
opi::prolog_impl::debug(value expr, value args)
{
  std::clog << "debug ";
  source_location location;
  if (get_location(expr, location))
    std::clog << display_location(location, 0, "\e[38;5;5;1m", "\e[2m");
  for (const value x : range(args))
    display(std::clog, reconstruct(x, stringify_unbound_variables));
  std::clog << std::endl;
}

opi::value
opi::prolog_impl::elements_of(value l)
{
  value acc = nil;
  value mem = nil;
  for (; not memq(l, mem) and l->t == tag::pair; mem = cons(l, mem), l = cdr(l))
    acc = cons(car(l), acc);
  return acc;
}

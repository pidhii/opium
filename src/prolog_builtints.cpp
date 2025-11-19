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


#include "opium/prolog.inl"


bool
opi::prolog_impl::var(value x)
{
  value _ = nil;
  if (ispair(x) and is(car(x), cell_tag))
    return not get_value(static_cast<cell*>(ptr_val(cdr(x))), _);
  return false;
}

void
opi::prolog_impl::debug(value expr, value args)
{
  std::clog << "debug ";
  source_location location;
  if (get_location(expr, location))
    std::clog << display_location(location, 0, "\e[38;5;5;1m", "\e[2m") << " ";
  for (const value x : range(args))
    display(std::clog, reconstruct(x, stringify_unbound_variables));
  std::clog << std::endl;
}

opi::value
opi::prolog_impl::elements_of(value l)
{
  value acc = nil;
  value mem = nil;
  for (; not memq(l, mem) and ispair(l); mem = cons(l, mem), l = cdr(l))
    acc = cons(car(l), acc);
  return acc;
}

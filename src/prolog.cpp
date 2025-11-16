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


#include "opium/prolog.hpp"
#include "opium/source_location.hpp"
#include "opium/value.hpp"

#include <regex.h>


opi::prolog::prolog()
: m_depth {0}, m_trace {make<trace_node>()}, m_cut {false}, m_cutpred {false}
{
  // Define `=` as builtin
  add_predicate(list("=", "X", "X"), True);
}


const opi::predicate&
opi::prolog::add_predicate(const predicate &pred)
{ return m_db.emplace(pred.name(), pred)->second; }


const opi::predicate&
opi::prolog::add_predicate(value sig, value body)
{ return add_predicate(predicate {sig, body}); }


void
opi::prolog::_trace_expr(value expr) const
{
  source_location location;
  trace_node *newtracenode = make<trace_node>();
  if (get_location(expr, location))
    newtracenode->location = std::move(location);
  else
    newtracenode->location = source_location {"<string>", 0, 0};
  m_trace->children.push_back(newtracenode);
  m_trace = newtracenode;
}

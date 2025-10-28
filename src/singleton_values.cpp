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


#include "opium/value.hpp"


////////////////////////////////////////////////////////////////////////////////
//
//                             Booleans
//
static opi::object*
_initialize_boolean(opi::object *ptr, bool val)
{
  ptr->_t = opi::tag::boolean;
  ptr->boolean = val;
  return ptr;
}

static
opi::object True_object {opi::tag::boolean}, False_object {opi::tag::boolean};
const opi::value opi::True {_initialize_boolean(&True_object, true)},
                opi::False {_initialize_boolean(&False_object, false)};

////////////////////////////////////////////////////////////////////////////////
//
//                              Nil
//
static
opi::object nil_object {opi::tag::nil};
const opi::value opi::nil {&nil_object};

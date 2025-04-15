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


#pragma once

#include <tuple>


namespace opi::utl {

template <typename ...T>
struct state_saver {
  state_saver(T &...refs): m_save {refs...}, m_refs {refs...} {}

  ~state_saver()
  { m_refs = m_save; }

  std::tuple<T...> m_save;
  std::tuple<T&...> m_refs;
}; // struct state_saver

} // namespace opi::utl
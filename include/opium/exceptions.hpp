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

#include "opium/value.hpp"
#include "opium/source_location.hpp"

#include <stdexcept>
#include <ostream>
#include <sstream>
#include <optional>


namespace opi {

struct bad_code: std::runtime_error {
  bad_code(std::string_view what): runtime_error(std::string(what)) { }
  bad_code(std::string_view what, const source_location &location);
  bad_code(std::string_view what, value code);

  void
  display(std::ostream &os) const noexcept;

  std::string
  display() const
  {
    std::ostringstream buf;
    display(buf);
    return buf.str();
  }

  private:
  std::optional<source_location> m_location;
}; // struct opi::bad_code

} // namespace opi
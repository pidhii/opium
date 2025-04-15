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


#include "opium/exceptions.hpp"
#include "opium/source_location.hpp"


opi::bad_code::bad_code(std::string_view what, value code)
: runtime_error(std::string(what))
{
  source_location location;
  if (get_location(code, location))
    m_location = location;
}


opi::bad_code::bad_code(std::string_view what, const source_location &location)
: runtime_error(std::string(what)), m_location {location}
{ }



void
opi::bad_code::display(std::ostream &os) const noexcept
{
  // Write basic error report
  os << what();
  
  // Write location if available
  if (m_location)
    os << "\n" << display_location(m_location.value());
}

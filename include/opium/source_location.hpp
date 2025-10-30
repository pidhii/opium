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
#include "opium/stl/string.hpp"

#include <string>
#include <string_view>

/**
 * \file source_location.hpp
 * Source location tracking for Lisp values
 * 
 * This file defines utilities for tracking and displaying source locations
 * of parsed Lisp expressions.
 * 
 * \ingroup lisp
 */

namespace opi {

/**
 * Structure representing a location in the input stream
 * 
 * \ingroup lisp
 */
struct source_location {
  source_location() = default;

  source_location(const source_location &other) = default;

  source_location(const std::string_view &source_, size_t start, size_t end)
  : source {source_.data()},
    start {start},
    end {end}
  {
    assert(strlen(source.c_str()) == source.size());
  }

  source_location&
  operator = (const source_location &other) = default;

  bool
  operator == (const source_location &other)
  { return source == other.source and start == other.start and end == other.end; }

  stl::string source; ///< Source name (filepath or "<string>")
  size_t start; ///< Start offset in the input stream
  size_t end;   ///< End offset in the input stream
};

/**
 * Set the source location for a value
 * 
 * \param val Value to set the location for
 * \param loc Source location
 */
void
set_location(value val, const source_location &loc);

/**
 * Get the source location for a value
 * 
 * \param val Value to get the location for
 * \param[out] location Source location if available
 * \return True if location for \p val is available
 */
[[nodiscard]] bool
get_location(value val, source_location &location);

/**
 * Check if a value has a source location
 * 
 * \param val Value to check
 * \return True if \p val has a source location
 */
[[nodiscard]] inline bool
has_location(value val)
{ return val->location != nullptr; }

/**
 * Copy the source location to another value
 *
 * \param from Value to copy the location from
 * \param to Value to copy the location to
 * \return True if location was copied or if \p from and \p to is the same object
 */
bool
copy_location(opi::value from, opi::value to);

/**
 * Display a fragment of a file according to location with surrounding context
 * and highlighting of the location region
 * 
 * \param location Source location to display
 * \param context_lines Number of context lines to show before and after the location
 * \return Formatted string with the file fragment and highlighting
 */
[[nodiscard]] std::string
display_location(const source_location &location, size_t context_lines = 2,
                 std::string_view hlstyle = "\e[38;5;1;1m",
                 std::string_view ctxstyle = "");

} // namespace opi

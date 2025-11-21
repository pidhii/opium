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


#include "opium/scheme/scheme_type_location_map.hpp"
#include "opium/logging.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/source_location.hpp"

#include <algorithm>
#include <iterator>
#include <format>
#include <vector>


namespace opi {

void
scheme_type_location_map::add(const source_location &loc, value type)
{ m_map.emplace(loc, type); }


bool
scheme_type_location_map::get_type(const source_location &loc,
                                   value &type) const
{
  auto it = m_map.find(loc);
  if (it != m_map.end())
  {
    type = it->second;
    return true;
  }
  return false;
}


bool
scheme_type_location_map::has_type(const source_location &loc) const
{ return m_map.find(loc) != m_map.end(); }


size_t
scheme_type_location_map::size() const
{ return m_map.size(); }


bool
scheme_type_location_map::empty() const
{ return m_map.empty(); }


void
scheme_type_location_map::clear()
{ m_map.clear(); }


/**
 * Recursively scan a value and its children for source locations and types
 * 
 * \param map The map to populate
 * \param prolog_emitter The prolog emitter containing type information
 * \param val The value to scan
 */
static void
scan_value_for_types(scheme_type_location_map &map,
                     const code_type_map &code_types, value val)
{
  // Check if this value has a source location
  source_location loc;
  if (get_location(val, loc))
  {
    // Try to get the type for this value
    value type = nil;
    if (code_types.code_type(val, type))
    {
      // Add the mapping to the map
      map.add(loc, type);
    }
  }

  // Recursively scan children if this is a pair
  if (opi::ispair(val))
  {
    scan_value_for_types(map, code_types, car(val));
    scan_value_for_types(map, code_types, cdr(val));
  }
}


scheme_type_location_map
build_type_location_map(const code_type_map &code_types, value ppcode)
{
  scheme_type_location_map map;

  // Scan each top-level expression in the preprocessed code
  for (const value expr : range(ppcode))
    scan_value_for_types(map, code_types, expr);

  debug("Built type location map with {} entries", map.size());
  return map;
}

std::string
scheme_type_location_map::display_source_with_types(
    std::string_view source, std::istream &in, int start_offset, int end_offset,
    std::string_view type_style) const
{
  // Read the entire file content
  std::string content {std::istreambuf_iterator<char>(in),
                       std::istreambuf_iterator<char>()};

  if (start_offset < 0)
    start_offset = 0;
  if (end_offset < 0)
    end_offset = content.size();

  // Cut out location delimited by `start_offset` and `end_offset`
  content.erase(end_offset);
  content.erase(0, start_offset);
  
  // Create a vector of type annotations sorted by end position
  struct type_annotation {
    size_t start;
    size_t end;
    value type;
  };

  std::vector<type_annotation> annotations;
  for (const auto &[loc, type] : m_map)
  {
    if (loc.source == source and loc.start >= size_t(start_offset) and
        loc.end <= size_t(end_offset))
      annotations.emplace_back(loc.start - start_offset, loc.end - start_offset,
                               type);
  }

  // Sort annotations by end position (descending) to process from end to start
  std::sort(annotations.begin(), annotations.end(),
            [](const type_annotation &a, const type_annotation &b) {
              return a.end > b.end;
            });

  // Insert type annotations
  for (const auto &ann : annotations)
  {
    std::ostringstream buf;
    pretty_template_instance_name(ann.type, buf);
    std::string type_annotation =
        std::format("{}::{}\e[0m", type_style, buf.str());
    content.insert(ann.end, type_annotation);
  }

  return content;
}

std::string
scheme_type_location_map::display_location_with_types(
    const source_location &location, std::string_view type_style) const
{
  const auto [start, end] = compute_linespan_indices(location, 1);
  info("linespan indices: {} - {}", start, end);
  std::ifstream file {location.source.c_str()};
  return display_source_with_types(location.source, file, start, end,
                                   type_style);
}

} // namespace opi

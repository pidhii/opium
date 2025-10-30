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
                     const scheme_to_prolog &prolog_emitter, value val)
{
  // Check if this value has a source location
  source_location loc;
  if (get_location(val, loc))
  {
    // Try to get the type for this value
    value type = nil;
    if (prolog_emitter.find_code_type(val, type))
    {
      // Add the mapping to the map
      map.add(loc, type);
    }
  }

  // Recursively scan children if this is a pair
  if (opi::ispair(val))
  {
    scan_value_for_types(map, prolog_emitter, car(val));
    scan_value_for_types(map, prolog_emitter, cdr(val));
  }
}


scheme_type_location_map
build_type_location_map(const scheme_to_prolog &prolog_emitter, value ppcode)
{
  scheme_type_location_map map;

  // Scan each top-level expression in the preprocessed code
  for (const value expr : range(ppcode))
    scan_value_for_types(map, prolog_emitter, expr);

  debug("Built type location map with {} entries", map.size());
  return map;
}


void
scheme_type_location_map::display_source_with_types(
    std::string_view source, std::istream &in, std::ostream &out,
    std::string_view type_style) const
{
  // Read the entire file content
  std::string content {std::istreambuf_iterator<char>(in),
                       std::istreambuf_iterator<char>()};

  // Create a vector of type annotations sorted by end position
  struct type_annotation {
    size_t start;
    size_t end;
    value type;
  };

  std::vector<type_annotation> annotations;
  for (const auto &[loc, type] : m_map)
  {
    if (loc.source == source)
      annotations.emplace_back(type_annotation {loc.start, loc.end, type});
  }

  // Sort annotations by end position (descending) to process from end to start
  std::sort(annotations.begin(), annotations.end(),
            [](const type_annotation &a, const type_annotation &b) {
              return a.end > b.end;
            });

  // Insert type annotations
  std::string result = content;
  for (const auto &ann : annotations)
  {
    std::ostringstream buf;
    pretty_template_instance_name(ann.type, buf);
    std::string type_annotation =
        std::format("{}:{}\e[0m", type_style, buf.str());
    result.insert(ann.end, type_annotation);
  }

  out << result;
}

} // namespace opi

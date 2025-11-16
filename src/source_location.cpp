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


#include "opium/source_location.hpp"
#include "opium/format.hpp" // IWYU pragma: keep
#include "opium/logging.hpp"

#include <fstream>
#include <sstream>
#include <vector>


// Global map to store value locations
// TODO: make it a weak-map
// static opi::stl::unordered_map<opi::object *, opi::source_location>
//     g_value_locations;

bool
opi::get_location(opi::value val, opi::source_location &location)
{
  if (val->location == nullptr)
    return false;
  
  location = *val->location;
  return true;
}

void
opi::set_location(opi::value val, const opi::source_location &loc)
{
  if (val != nil and val != True and val != False)
    val->location = make<source_location>(loc);
}

bool
opi::copy_location(opi::value from, opi::value to)
{
  if (is(to, nil) or is(to, True) or is(to, False) or has_location(to))
    return false;

  return bool(to->location = from->location);
}

std::string
opi::display_location(const opi::source_location &location,
                      size_t context_lines, std::string_view hlstyle,
                      std::string_view ctxstyle, std::string_view endstyle)
{
  // If the source is not a file, handle it differently
  if (location.source[0] == '<')
    return std::format("in {}: offset {} to {}", location.source,
                       location.start, location.end);

  // Try to open the file
  std::ifstream file {location.source.data()};
  if (not file.is_open())
    return std::format("Could not open file: {}", location.source);

  // Read the entire file content
  std::string content {std::istreambuf_iterator<char>(file),
                       std::istreambuf_iterator<char>()};
  file.close();

  // Find line and column information
  std::vector<size_t> line_starts;
  line_starts.push_back(0); // First line starts at offset 0

  for (size_t i = 0; i < content.size(); ++i)
  {
    if (content[i] == '\n')
      line_starts.push_back(i + 1);
  }

  // Find the line containing the start position
  size_t start_line = 0;
  while (start_line < line_starts.size() &&
         line_starts[start_line] <= location.start)
  {
    start_line++;
  }
  start_line = start_line > 0 ? start_line - 1 : 0;

  // Find the line containing the end position
  size_t end_line = start_line;
  while (end_line < line_starts.size() and line_starts[end_line] <= location.end)
    end_line++;
  end_line = end_line > 0 ? end_line - 1 : 0;

  // Calculate the range of lines to display
  size_t display_start =
      start_line > context_lines ? start_line - context_lines : 0;
  size_t display_end =
      std::min(end_line + context_lines, line_starts.size() - 1);

  // Build the output
  std::ostringstream output;
  output << std::format("in {}:{}:{} to {}:{}\n", location.source,
                        start_line + 1,
                        location.start - line_starts[start_line] + 1,
                        end_line + 1, location.end - line_starts[end_line] + 1);

  // Display the lines with context
  for (size_t i = display_start; i <= display_end; ++i)
  {
    // Calculate the end of this line
    size_t line_end =
        (i + 1 < line_starts.size()) ? line_starts[i + 1] - 1 : content.size();
    if (line_end > 0 && content[line_end - 1] == '\r')
      line_end--; // Handle CRLF line endings

    // Extract the line content
    std::string line =
        content.substr(line_starts[i], line_end - line_starts[i]);

    // Format the line number
    output << std::format("{:4d} | ", i + 1) << ctxstyle;

    // If this line contains the highlighted region
    if (i >= start_line && i <= end_line)
    {
      if (start_line == end_line)
      { // Highlight is within a single line
        size_t start_col = location.start - line_starts[start_line];
        size_t end_col = location.end - line_starts[start_line];

        // Output the line with highlighting
        output << line.substr(0, start_col);
        output << endstyle << hlstyle;
        output << line.substr(start_col, end_col - start_col);
        output << endstyle << ctxstyle; // Reset formatting
        output << line.substr(end_col);
      }
      else if (i == start_line)
      { // First line of multi-line highlight
        size_t start_col = location.start - line_starts[start_line];

        output << line.substr(0, start_col);
        output << endstyle << hlstyle;
        output << line.substr(start_col);
        output << endstyle << ctxstyle; // Reset formatting
      }
      else if (i == end_line)
      { // Last line of multi-line highlight
        size_t end_col = location.end - line_starts[end_line];

        output << endstyle << hlstyle;
        output << line.substr(0, end_col);
        output << endstyle << ctxstyle; // Reset formatting
        output << line.substr(end_col);
      }
      else
      { // Middle line of multi-line highlight
        output << endstyle << hlstyle;
        output << line;
        output << endstyle << ctxstyle; // Reset formatting
      }
    }
    else
    { // Regular line, no highlighting
      output << line;
    }

    output << endstyle << "\n";
  }

  return output.str();
}

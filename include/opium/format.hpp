#pragma once

#include "opium/value.hpp"

#include <sstream>
#include <format>

/**
 * \file format.hpp
 * Formatting utilities
 * 
 * This file defines utilities for formatting values as strings.
 * 
 * \ingroup utils
 */


namespace opi {

/**
 * \namespace opi::detail
 * Implementation details
 */
namespace detail {

/**
 * Format a single value to an output stream
 * 
 * \tparam Os Output stream type
 * \tparam Head Value type
 * \param os Output stream
 * \param head Value to format
 * 
 * \ingroup utils
 */
template <typename Os, typename Head>
void
format(Os &os, Head&& head)
{ os << std::forward<Head>(head); }

/**
 * Format multiple values to an output stream
 * 
 * \tparam Os Output stream type
 * \tparam Head First value type
 * \tparam Tail Rest of value types
 * \param os Output stream
 * \param head First value to format
 * \param tail Rest of values to format
 * 
 * \ingroup utils
 */
template <typename Os, typename Head, typename ...Tail>
void
format(Os &os, Head&& head, Tail ...tail)
{
  os << std::forward<Head>(head);
  return format(os, std::forward<Tail>(tail)...);
}

} // namespace opi::detail

} // namespace opi


namespace std {

/**
 * Formatter for opi::value
 * 
 * \ingroup utils
 */
template <>
struct formatter<opi::value, char> {
  enum class style { write, display, print } style = style::print;
  unsigned long n = -1;

  template <class ParseContext>
  constexpr ParseContext::iterator
  parse(ParseContext &ctx)
  {
    auto it = ctx.begin();
    while (it != ctx.end())
    {
      if (*it == 'w')
      {
        style = style::write;
        it++;
      }

      if (*it == 'd')
      {
        style = style::display;
        it++;
      }
      
      if (*it == '#')
      {
        it++;
        std::string buf;
        while (std::isdigit(*it))
          buf.push_back(*it++);
        n = std::stoul(buf);
      }

      break;
    }
    if (it != ctx.end() and *it != '}')
      throw std::format_error {"Invalid format arguments for opi::value"};

    return it;
  }

  template <class FmtContext>
  FmtContext::iterator
  format(opi::value x, FmtContext &ctx) const
  {
    std::ostringstream buffer;
    switch (style)
    {
      case style::write: opi::write(buffer, x); break;
      case style::display: opi::display(buffer, x); break;
      case style::print: opi::print(buffer, x); break;
    }
    return std::ranges::copy(std::move(buffer).str(), ctx.out()).out;
  }
};

} // namespace std

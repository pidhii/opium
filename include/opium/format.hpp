#pragma once

#include "opium/value.hpp"

#include <string>
#include <sstream>
#include <format>


namespace opi {

namespace detail {

template <typename Os, typename Head>
void
format(Os &os, Head&& head)
{ os << std::forward<Head>(head); }

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

template <>
struct formatter<opi::value, char> {
  // Unused for now
  enum class style { write, display } style = style::write;

  template <class ParseContext>
  constexpr ParseContext::iterator
  parse(ParseContext &ctx)
  {
    auto it = ctx.begin();
    if (it == ctx.end())
      return it;

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

    if (it != ctx.end() and *it != '}')
      throw std::format_error {"Invalid format arguments for opi::value"};

    return it;
  }

  template <class FmtContext>
  FmtContext::iterator
  format(opi::value x, FmtContext &ctx) const
  {
    std::ostringstream buffer;
    opi::detail::format(buffer, x);
    return std::ranges::copy(std::move(buffer).str(), ctx.out()).out;
  }
};

} // namespace std
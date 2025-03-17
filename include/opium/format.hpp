#pragma once

#include <string>
#include <sstream>

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


template <typename ...Tail>
std::string
format(Tail&& ...tail)
{
  std::ostringstream buf;
  detail::format(buf, std::forward<Tail>(tail)...);
  return std::move(buf).str();
}



} // namespace opi

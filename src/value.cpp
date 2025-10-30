#include "opium/value.hpp"
#include "opium/source_location.hpp"
#include "opium/stl/unordered_set.hpp"
#include <utility>


bool
opi::g_propagate_locations_on_cons = true;

static
opi::stl::unordered_set<std::string_view> g_const_strings;


std::string_view
opi::global_string(std::string_view str) noexcept
{
  const auto it = g_const_strings.find(str);
  if (it != g_const_strings.end())
    return *it;

  char *gstr = static_cast<char*>(opi::allocate(str.size() + 1));
  str.copy(gstr, str.size());
  gstr[str.size()] = '\0';
  g_const_strings.emplace(gstr, str.size());
  return {gstr, str.size()};
}


opi::value
opi::cons(value car, value cdr)
{
  const value result = pair(car, cdr);
  if (not g_propagate_locations_on_cons)
    return result;

  source_location carlocation, cdrlocation;
  const bool hascarlocation = car != nil and get_location(car, carlocation);
  const bool hascdrlocation = cdr != nil and get_location(cdr, cdrlocation);

  if (hascarlocation and hascdrlocation)
  {
    if (carlocation.source == cdrlocation.source)
    {
      source_location location;
      location.source = carlocation.source;
      location.start = carlocation.start;
      location.end = cdrlocation.end;
      if (location.start > location.end)
        std::swap(location.start, location.end);
      set_location(result, location);
    }
  }
  else if (hascarlocation)
  {
    source_location location;
    location.source = carlocation.source;
    location.start = carlocation.start;
    location.end = carlocation.end;
    if (location.start > location.end)
      std::swap(location.start, location.end);
    set_location(result, location);
  }
  // else if (hascdrlocation)
  // {
  //   source_location location;
  //   location.source = cdrlocation.source;
  //   location.start = cdrlocation.start;
  //   location.end = cdrlocation.end;
  //   if (location.start > location.end)
  //     std::swap(location.start, location.end);
  //   set_location(result, location);
  // }

  return result;
}

opi::value
opi::from(const std::pair<value, value> &p)
{ return cons(p.first, p.second); }

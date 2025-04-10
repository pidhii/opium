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

#pragma once

#include "opium/value.hpp"
#include "opium/source_location.hpp"

#include <stdexcept>
#include <ostream>
#include <sstream>
#include <optional>


namespace opi {

struct bad_code: std::runtime_error {
  bad_code(std::string_view what): runtime_error(std::string(what)) { }
  bad_code(std::string_view what, const source_location &location);
  bad_code(std::string_view what, value code);

  void
  display(std::ostream &os) const noexcept;

  std::string
  display() const
  {
    std::ostringstream buf;
    display(buf);
    return buf.str();
  }

  private:
  std::optional<source_location> m_location;
}; // struct opi::bad_code

} // namespace opi
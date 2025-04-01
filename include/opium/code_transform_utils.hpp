#pragma once

#include "opium/value.hpp"

#include <string_view>
#include <string>
#include <format>


namespace opi {

class symbol_generator {
  public:
  symbol_generator(size_t &counter, std::string_view format = "_Uid{}")
  : m_format {format}, m_counter {counter}
  { }

  value
  operator () ()
  {
    m_counter ++;
    return sym(std::vformat(m_format, std::make_format_args(m_counter)));
  }

  private:
  const std::string m_format;
  size_t &m_counter;
}; // class opi::symbol_generator

} // namespace opi
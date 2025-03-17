#pragma once

#include "opium/format.hpp"

#include <iostream>


namespace opi {


extern size_t logging_indent;


struct add_indent {
  add_indent(size_t indent): m_indent {indent} { }

  inline friend std::ostream&
  operator << (std::ostream &os, const add_indent &self) noexcept
  {
    for (size_t i = 0; i < self.m_indent; ++i)
      os << "| ";
    return os;
  }

  private:
  size_t m_indent;
};


template <typename... Args> void
debug([[maybe_unused]] Args &&...args)
{
  // std::cerr << "[\e[7;1m debg \e[0m] " << add_indent(logging_indent)
  //           << format(std::forward<Args>(args)...) << std::endl;
}

template <typename... Args> void
info(Args &&...args)
{
  std::cerr << "[ info ] " << add_indent(logging_indent)
            << format(std::forward<Args>(args)...) << std::endl;
}

template <typename... Args> void
error(Args &&...args)
{
  std::cerr << "[\e[38;5;1;1m fail \e[0m] " << add_indent(logging_indent)
            << format(std::forward<Args>(args)...) << std::endl;
}

template <typename... Args> void
warning(Args &&...args)
{
  std::cerr << "[\e[38;5;3;1m warn \e[0m] " << add_indent(logging_indent)
            << format(std::forward<Args>(args)...) << std::endl;
}

struct indent {
  indent(size_t inc = 1)
  : m_inc {inc}
  { logging_indent += m_inc; }

  ~indent()
  { logging_indent -= m_inc; }

  indent(const indent&) = delete;
  void operator = (const indent&) = delete;

  private:
  size_t m_inc;
}; // struct opi::indent

} // namespace opi

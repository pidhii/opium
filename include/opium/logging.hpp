#pragma once

#include "opium/format.hpp" // IWYU pragma: export

#include <format>
#include <iostream>
#include <stdexcept>


namespace opi {


enum class loglevel: int {
  silent,
  error,
  warning,
  info,
  debug,
};

inline std::string_view
loglevel_name(loglevel lvl)
{
  switch (lvl)
  {
    case loglevel::silent: return "silent";
    case loglevel::error: return "error";
    case loglevel::warning: return "warning";
    case loglevel::info: return "info";
    case loglevel::debug: return "debug";
  }
  std::terminate();
}

inline loglevel
parse_loglevel(std::string_view name)
{
  if (name == "silent")
    return loglevel::silent;
  if (name == "error")
    return loglevel::error;
  if (name == "warning")
    return loglevel::warning;
  if (name == "info")
    return loglevel::info;
  if (name == "debug")
    return loglevel::debug;
  throw std::runtime_error {std::format("Invalid loglevel name ({})", name)};
}


inline bool
operator >= (loglevel a, loglevel b)
{ return static_cast<int>(a) >= static_cast<int>(b); }


extern size_t logging_indent;

extern loglevel loglevel;


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
debug(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::debug)
  {
    std::cerr << "opium \e[7;1mdebug\e[0m " << add_indent(logging_indent);
    std::cerr << std::format(fmt, std::forward<Args>(args)...) << std::endl;
  }
}


template <typename... Args> void
info(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::info)
  {
    std::cerr << "opium " << add_indent(logging_indent);
    std::cerr << std::format(fmt, std::forward<Args>(args)...) << std::endl;
  }
}


template <typename... Args> void
warning(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::warning)
  {
    std::cerr << "opium \e[38;5;3;1mwarning\e[0m " << add_indent(logging_indent);
    std::cerr << std::format(fmt, std::forward<Args>(args)...) << std::endl;
  }
}


template <typename... Args> void
error(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::error)
  {
    std::cerr << "opium \e[38;5;1;1merror\e[0m " << add_indent(logging_indent);
    std::cerr << std::format(fmt, std::forward<Args>(args)...) << std::endl;
  }
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

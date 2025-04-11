#pragma once

#include "opium/format.hpp" // IWYU pragma: export

#include <format>
#include <iostream>
#include <stdexcept>
#include <set>


namespace opi {

extern
std::set<std::string> global_flags;


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
    for (size_t i = 1; i < self.m_indent; ++i)
      os << "\e[2m¦\e[0m ";
    if (self.m_indent > 0)
      os << "| ";
    return os;
  }

  private:
  size_t m_indent;
};

/**
 * Insert indentation for each line of multiline string
 */
template <typename Indent>
[[nodiscard]] std::string
indent_lines(Indent&& indent, const std::string &string)
{
  std::istringstream input {string};
  std::ostringstream output;
  std::string line;
  while (std::getline(input, line))
    output << std::forward<Indent>(indent) << line << "\n";
  return output.str();
}


template <typename... Args> void
debug([[maybe_unused]] std::format_string<Args...> fmt, [[maybe_unused]] Args &&...args)
{
#ifndef OPIUM_RELEASE_BUILD
  if (loglevel >= loglevel::debug)
  {
    const std::string message = std::format(fmt, std::forward<Args>(args)...);
    const auto indent = add_indent(logging_indent + 7 /* for log name */);
    const std::string indented_message = indent_lines(indent, message);
    std::cerr << "opium \e[7;1mdebug\e[0m"
              << indented_message.substr(65, std::string::npos);
  }
#endif
}


template <typename... Args> void
info(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::info)
  {
    const std::string message = std::format(fmt, std::forward<Args>(args)...);
    const auto indent = add_indent(logging_indent + 4 /* for log name */);
    const std::string indented_message = indent_lines(indent, message);
    std::cerr << "opium " << indented_message.substr(33, std::string::npos);
  }
}


template <typename... Args> void
warning(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::warning)
  {
    const std::string message = std::format(fmt, std::forward<Args>(args)...);
    const auto indent = add_indent(logging_indent + 8 /* for log name */);
    const std::string indented_message = indent_lines(indent, message);
    std::cerr << "opium \e[38;5;3;1mwarning\e[0m"
              << indented_message.substr(76, std::string::npos);
  }
}


template <typename... Args> void
error(std::format_string<Args...> fmt, Args &&...args)
{
  if (loglevel >= loglevel::error)
  {
    const std::string message = std::format(fmt, std::forward<Args>(args)...);
    const auto indent = add_indent(logging_indent + 7 /* for log name */);
    const std::string indented_message = indent_lines(indent, message);
    std::cerr << "opium \e[38;5;1;1merror\e[0m"
              << indented_message.substr(65, std::string::npos);
  }
}


struct indent {
  indent(ssize_t inc = 1)
  : m_inc {inc}
  { logging_indent += m_inc; }

  ~indent()
  { logging_indent -= m_inc; }

  indent(const indent&) = delete;
  void operator = (const indent&) = delete;

  private:
  ssize_t m_inc;
}; // struct opi::indent

} // namespace opi

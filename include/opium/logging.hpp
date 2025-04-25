/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#pragma once

#include "opium/format.hpp" // IWYU pragma: export

#include <format>
#include <iostream>
#include <stdexcept>
#include <set>
#include <regex>


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
 * Strip ANSI escape sequences from a string
 * 
 * \param input The input string containing escape sequences
 * \return The string with escape sequences removed
 */
inline std::string
strip_escape_sequences(const std::string &input)
{
  // Regex to match escape sequences starting with '\e' and ending with 'm'
  static const std::regex escape_seq_regex("\\\e\\[[^m]*m");
  return std::regex_replace(input, escape_seq_regex, "");
}


/**
 * Insert indentation for each line of multiline string
 */
template <typename Indent>
[[nodiscard]] std::string
indent_lines(Indent &&indent, const std::string &string,
             [[maybe_unused]] size_t max_length = 0)
{
  std::istringstream input {string};
  std::ostringstream output;
  std::string line;
  while (std::getline(input, line))
  {
#ifndef OPIUM_RELEASE_BUILD
    std::string strippedline = strip_escape_sequences(line);
    if (max_length > 0 and strippedline.length() > max_length)
    {
      strippedline.erase(max_length - 1);
      std::ranges::copy("…", std::back_inserter(strippedline));
      output << std::forward<Indent>(indent) << strippedline << "\n";
    }
    else
#endif
      output << std::forward<Indent>(indent) << line << "\n";
  }
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
    const std::string indented_message = indent_lines(indent, message, 150);
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
    const std::string indented_message = indent_lines(indent, message, 150);
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
    const std::string indented_message = indent_lines(indent, message, 150);
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
    const std::string indented_message = indent_lines(indent, message, 150);
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

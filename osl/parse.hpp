#pragma once

#include "osl_parser.hpp" // FIXME

#include "opium/source_location.hpp"
#include "opium/stl/deque.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/value.hpp"
#include "opium/hash.hpp"

#include <readline/history.h>
#include <string>
#include <stack>


namespace opi::osl {

using scanner = void*;

extern std::vector<std::string> pathes;

int
yylex(YYSTYPE *yylval_p, YYLTYPE *loc_p, yyscan_t yyscanner);


class lexer {
  public:
  struct token {
    source_location location;
    opi::value value;
    int type;
  }; // struct opi::osl::token

  lexer(const source_location &start_location, FILE *input);

  ~lexer();

  void*
  yyhandle() const noexcept
  { return m_yyscanner; }

  int
  read(token &result);

  void
  put(const token &token);

  void
  put(int token)
  { put({{}, nil, token}); }

  int
  peek(token &result);

  private:
  std::stack<token, stl::deque<token>> m_tokbuf;
  source_location m_location;
  void *m_yyscanner;
}; // class opi::osl::lexer



struct syntax;
struct macro {
  const syntax *pattern, *rule;
  opi::stl::unordered_map<opi::value, opi::value> paramtypes;
};
using macros_library = stl::unordered_map<value, macro>;


class parser {
  public:
  parser(int entry_token): m_entry_token {entry_token} { }

  opi::value
  parse(lexer &lexer, bool force = false);

  void
  add_macro(std::string_view name, const macro &macro)
  { m_macros.emplace(sym(name), macro); }

  void
  add_macro(std::string_view name, macro &&macro)
  { m_macros.emplace(sym(name), std::move(macro)); }

  private:
  macros_library m_macros;
  int m_entry_token;
}; // class opi::osl::parser


class program_sources {
  public:
  bool
  contains(const std::string &source_name) const noexcept
  {
    const auto first = [](const auto &pair) { return pair.first; };
    return std::ranges::find(m_sources, source_name, first) != m_sources.end();
  }

  void
  append(const std::string &source_name, value source_code)
  { m_sources.emplace_back(source_name, source_code); }

  value
  build_program() const
  {
    value merged_code;
    for (const auto &[source_name, code] : m_sources)
      merged_code = append_mut(merged_code, code);
    return merged_code;
  }

  private:
  stl::vector<std::pair<std::string, value>> m_sources;
};


// Auxiliary definitions used by parser
value
syntax_requirements();


class tree_parser: public parser {
  public:
  tree_parser(program_sources &target);

  void
  load_source(const std::string &source, std::FILE *file);

  void
  load_file(const std::string &path);

  program_sources &
  target() noexcept
  { return m_target; }

  const program_sources &
  target() const noexcept
  { return m_target; }

  private:
  program_sources &m_target;

  friend opi::value
  parse_current(yyscan_t scanner, std::optional<int> starttok,
                std::optional<range_location> startloc);
};

} // namespace opi::osl

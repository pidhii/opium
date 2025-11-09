#pragma once

#include "osl_parser.hpp" // FIXME

#include "opium/source_location.hpp"
#include "opium/stl/deque.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/value.hpp"
#include "opium/hash.hpp"
#include "opium/exceptions.hpp"

#include <readline/history.h>
#include <string>
#include <stack>


namespace opi::osl {

using scanner = void*;

extern std::vector<std::string> pathes;

int
yylex(YYSTYPE *yylval_p, YYLTYPE *loc_p, yyscan_t yyscanner);



struct generic_lexer {
  struct token {
    source_location location;
    opi::value value;
    int type;
  }; // struct opi::osl::token

  virtual int
  read(token &result) = 0;

  virtual void
  put(const token &token) = 0;

  void
  put(int token)
  { put({{"<injection>", 0, 0}, nil, token}); }

  int
  peek(token &result);
}; // class opi::osl::lexer


class lexer: public generic_lexer {
  public:
  lexer(const source_location &start_location, FILE *input);

  ~lexer();

  int
  read(token &result) override;

  void
  put(const token &token) override;

  using generic_lexer::put;

  private:
  std::stack<token, stl::deque<token>> m_tokbuf;
  source_location m_location;
  void *m_yyscanner;
}; // class opi::osl::lexer


class stateful_lexer: public generic_lexer {
  struct state_data {
    enum tag { root, read, put } tag;
    lexer::token token;
    state_data *prev {nullptr};
    state_data *next {nullptr};
  };

  public:
  using state = state_data*;

  stateful_lexer(generic_lexer &lexer)
  : m_lexer {lexer},
    m_state {make<state_data>(state_data::root, lexer::token {}, nullptr, nullptr)}
  { }

  state
  current_state() const noexcept
  { return m_state; }

  int
  read(token &result) override
  {
    const int toktype = m_lexer.read(result);
    state_data *new_state = make<state_data>(state_data::read, result, m_state, nullptr);
    m_state->next = new_state;
    m_state = new_state;
    return toktype;
  }

  void
  put(const token &token) override
  {
    m_lexer.put(token);
    state_data *new_state = make<state_data>(state_data::put, token, m_state, nullptr);
    m_state->next = new_state;
    m_state = new_state;
  }

  // Iterating from the end, erase all paris of nodes matching [p]->[r]
  void
  _erase_pr(state target_state, bool &isnop)
  {
    isnop = true;

    for (state st = target_state; st->next; st = st->next)
    {
      if (st->tag == state_data::put and st->next->tag == state_data::read)
      {
        const state left = st->prev;
        const state right = st->next->next;
        // [left]->[p]->[r]->[right] ~> [left]->[right]
        if (left != nullptr)
          left->next = right;
        if (right != nullptr)
          right->prev = left;
        isnop = false;
      }
    }
  }

  // Trim trailing [p]s
  void
  _trim_p(state target_state)
  {
    // find start of the trailing [p]s
    state trail_start;
    for (trail_start = target_state;
         trail_start and trail_start->tag != state_data::put;
         trail_start = trail_start->next)
      ;

    // read them all out
    lexer::token token;
    for (state st = trail_start; st; st = st->next)
      m_lexer.read(token);

    // and erase from the state-list
    if (trail_start)
    {
      assert(trail_start->prev);
      trail_start->prev->next = nullptr;
    }
  }

  void
  recover_state(state target_state)
  {
    bool isnop;
    do { _erase_pr(target_state, isnop); } while (not isnop);

    _trim_p(target_state);

    state last_read = target_state;
    for (; last_read->next; last_read = last_read->next)
      ;

    for (state st = last_read; st; st = st->prev)
    {
      assert(st->tag != state_data::put);
      if (st->tag == state_data::read)
        m_lexer.put(st->token);
    }
  }

  using generic_lexer::put;

  private:
  generic_lexer &m_lexer;
  state_data *m_state;
};



struct syntax;
struct macro_case {
  const syntax *pattern, *rule;
  opi::stl::unordered_map<opi::value, opi::value> paramtypes;
};
using macro = stl::vector<macro_case>;

using macro_key = std::pair<int, value>;

struct macro_key_hash {
  size_t
  operator () (const macro_key &key) const noexcept
  {
    size_t hash = 0;
    hash_combine(hash, key.first);
    hash_combine(hash, key.second);
    return hash;
  }
};

using macros_library = stl::unordered_multimap<macro_key, macro, macro_key_hash>;


/** Specific exception type raised by the parser */
struct parse_error: public bad_code {
  using bad_code::bad_code;
};

class parser {
  public:
  opi::value
  parse(int entry_token, generic_lexer &lexer, bool force = false);

  void
  add_macro(const macro_key &key, const macro &macro)
  { m_macros.emplace(key, macro); }

  void
  add_macro(const macro_key &key, macro &&macro)
  { m_macros.emplace(key, std::move(macro)); }

  private:
  macros_library m_macros;
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

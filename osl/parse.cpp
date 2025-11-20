#include "parse.hpp"
#include "syntax_definitions.h"
#include "osl_parser.hpp"

#include <filesystem>
#include <format>


extern int
yylex_init(yyscan_t *scanner);

extern void
yyset_in(FILE *in, yyscan_t scanner);

extern void
yylex_destroy(yyscan_t scanner);

extern int
opi::osl::yylex(opi::value *yylval_p, range_location *loc_p, yyscan_t yyscanner);


int
opi::osl::generic_lexer::peek(token &result)
{
  if (read(result) != 0)
    put(result);
  return result.type;
}

  
opi::osl::lexer::lexer(const source_location &start_location, FILE *input)
: m_location {start_location}
{
  yylex_init(&m_yyscanner);
  yyset_in(input, m_yyscanner);
}

opi::osl::lexer::~lexer()
{ yylex_destroy(m_yyscanner); }

int
opi::osl::lexer::read(token &result)
{
  if (not m_tokbuf.empty())
  {
    result = m_tokbuf.top();
    m_tokbuf.pop();
    return result.type;
  }

  range_location yyloc {m_location.start, m_location.end};
  result.type = yylex(&result.value, &yyloc, m_yyscanner);
  m_location = result.location = {m_location.source, yyloc.left, yyloc.right};
  return result.type;
}

void
opi::osl::lexer::put(const token &token)
{ m_tokbuf.push(token); }


int
opi::osl::stateful_lexer::read(token &result)
{
  const int toktype = m_lexer.read(result);
  state_data *new_state = make<state_data>(state_data::read, result, m_state, nullptr);
  m_state->next = new_state;
  m_state = new_state;
  return toktype;
}

void
opi::osl::stateful_lexer::put(const token &token)
{
  m_lexer.put(token);
  state_data *new_state = make<state_data>(state_data::put, token, m_state, nullptr);
  m_state->next = new_state;
  m_state = new_state;
}

// Iterating from the end, erase all paris of nodes matching [p]->[r]
void
opi::osl::stateful_lexer::_erase_pr(state target_state, bool &isnop)
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
opi::osl::stateful_lexer::_trim_p(state target_state)
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
opi::osl::stateful_lexer::recover_state(state target_state)
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


opi::osl::program_parser::program_parser(program_sources &target,
                                         scheme_translator &translator_config)
: m_target {target},
  m_translator_config {translator_config}
{
  translator_config.preprocessor.set_norename_prefix("norename#");
}

void
opi::osl::program_parser::load_file(const std::string &pathstr)
{
  const std::filesystem::path path = std::filesystem::absolute(pathstr);
  const std::filesystem::path dirpath = path.parent_path();

  if (FILE *file = fopen(path.c_str(), "r"))
  {
    const std::filesystem::path curpath = std::filesystem::current_path();
    if (dirpath != "")
      std::filesystem::current_path(dirpath);

    try { load_source(path, file); }
    catch (...) {
      std::filesystem::current_path(curpath);
      fclose(file);
      throw;
    }
    std::filesystem::current_path(curpath);
    fclose(file);
  }
  else
    throw std::runtime_error {
        std::format("Failed to open file {} for reading", path.c_str())};
}

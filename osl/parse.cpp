#include "parse.hpp"
#include "syntax_definitions.h"
#include "osl_parser.hpp"

#include "opium/lisp_parser.hpp"

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
opi::osl::lexer::peek(token &result)
{
  if (read(result) != 0)
    put(result);
  return result.type;
}


  
opi::value
opi::osl::syntax_requirements()
{
  opi::lisp_parser lispparser;
  const opi::value header = lispparser.parse_all(osl_syntax_definitions);
  return header;
}


opi::osl::tree_parser::tree_parser(program_sources &target)
: parser(PARSE_TOPLVL),
  m_target {target}
{ }

void
opi::osl::tree_parser::load_file(const std::string &pathstr)
{
  const std::filesystem::path path {pathstr};
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
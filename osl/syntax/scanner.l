%{

#include "osl_parser.hpp"
#include "opium/logging.hpp"
#include "opium/value.hpp"
#include "parse.hpp"


// Helper function to remove underscores from number strings
static std::string
remove_underscores(const char* text) {
  std::string result;
  for (const char* p = text; *p; ++p) {
    if (*p != '_') {
      result += *p;
    }
  }
  return result;
}


#define YY_DECL int opi::osl::yylex(opi::value *yylval_p, range_location *loc_p, yyscan_t yyscanner)
#define yylval (*yylval_p)

extern size_t g_fileoffs_left;
extern size_t g_fileoffs_right;
#define YY_USER_ACTION \
  loc_p->left = loc_p->right; \
  loc_p->right = loc_p->left + yyleng;


namespace opi::osl {
opi::source_location
make_location(const range_location &loc);

std::optional<int> start_token;
}


%}

%option noyywrap
%option reentrant
%option interactive
/*%option c++*/

%x RAWSTRING
%x STRING
%x PRAGMA
%x FOREIGN_INLINE
%x SCHEME_INLINE

OPCHAR [-+*/=!?.:$%\\&\|<>]

%%

^"#!".*($|[\r\n])
"//".*($|[\r\n])?
[[:space:]]

<*>"\\"\n

require { yylval = "require"; return REQUIRE; }


^"```".*$ {
  BEGIN(FOREIGN_INLINE);
  return FOREIGN_INLINE_BEGIN;
}
<FOREIGN_INLINE>^"```"$ {
  BEGIN(INITIAL);
  return FOREIGN_INLINE_END;
}
<FOREIGN_INLINE>^\n {
  yylval = str("\n");
  return STRING_CHUNK;
}
<FOREIGN_INLINE>^.+$ {
  yylval = str(yytext);
  return STRING_CHUNK;
}
<FOREIGN_INLINE>\n


^"~~~".*$ {
  BEGIN(SCHEME_INLINE);
  return SCHEME_INLINE_BEGIN;
}
<SCHEME_INLINE>^"~~~"$ {
  BEGIN(INITIAL);
  return SCHEME_INLINE_END;
}
<SCHEME_INLINE>^.+$ {
  yylval = str(yytext);
  return STRING_CHUNK;
}
<SCHEME_INLINE>\n

scm[[:space:]]*"{"[^}]*"}" {
  const std::string data {strchr(yytext, '{') + 1, strchr(yytext, '}')};
  yylval = opi::str(data);
  return INLINE_SCHEME;
}

foreign { yylval = "foreign"; return FOREIGN; }

assert { yylval = "assert"; return ASSERT; }

macro { yylval = "macro"; return MACRO; }

infix[[:space:]]+left { yylval = sym(yytext); return INFIX_LEFT; }
infix[[:space:]]+right { yylval = sym(yytext); return INFIX_RIGHT; }
prefix { yylval = sym(yytext); return PREFIX; }
postfix { yylval = sym(yytext); return POSTFIX; }

match { yylval = "match"; return MATCH; }
in    { yylval = "in"; return IN; }

if { yylval = "if"; return IF; }
then { yylval = "then"; return THEN; }
else { yylval = "else"; return ELSE; }

otherwize { yylval = "otherwize"; return OTHERWIZE; }

and { yylval = "and"; return AND; }
or { yylval = "or"; return OR; }
not { yylval = "not"; return NOT; }

let { yylval = "let"; return LET; }
letrec { yylval = "letrec"; return LETREC; }
where { yylval = "where"; return WHERE; }

set { yylval = "set"; return SET; }

type { yylval = "type"; return TYPE; }

fn { yylval = "fn"; return FN; }

extern { yylval = "extern"; return EXTERN; }
as { yylval = "as"; return AS; }

explicit { yylval = "explicit"; return EXPLICIT; }

convert { yylval = "convert"; return CONVERT; }

identifier"!("[^)]+")" {
  // Read whatever is between the parenthes
  std::string text = yytext + strlen("identifier!(");
  text.pop_back();

  // Trim leading and trailing whitespaces
  while (text.size() > 0 and std::isspace(text.front()))
    text.erase(text.begin());
  while (text.size() > 0 and std::isspace(text.back()))
    text.pop_back();

  yylval = sym(text);
  return IDENT;
}

"("\s*[-+=!*/%<>:&]+\s*")" {
  std::string ident {yytext + 1, yytext + yyleng - 1};
  while (std::isspace(ident.front()))
    ident.erase(ident.begin());
  while (std::isspace(ident.back()))
    ident.pop_back();
  yylval = sym(ident);
  return IDENT;
}


\" {
  BEGIN(STRING);
  yylval = sym("\"");
  return BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
  return STRING_CHUNK;
}
<STRING>\\['\"\\nabfnrtve] {
  switch (yytext[1])
  {
    case '\'': yylval = str("'"); break;
    case '"': yylval = str("\""); break;
    case '\\': yylval = str("\\"); break;
    case '\n': yylval = str(""); break;
    case 'a': yylval = str("\a"); break;
    case 'b': yylval = str("\b"); break;
    case 'f': yylval = str("\f"); break;
    case 'n': yylval = str("\n"); break;
    case 'r': yylval = str("\r"); break;
    case 't': yylval = str("\t"); break;
    case 'v': yylval = str("\v"); break;
    case 'e': yylval = str("\e"); break;
  }
  return STRING_CHUNK;
}
<STRING>\\. {
  yylval = str(yytext+1);
  return STRING_CHUNK;
}
<STRING>\" {
  BEGIN(INITIAL);
  yylval = sym("\"");
  return END_STRING;
}

r\" {
  BEGIN(RAWSTRING);
  yylval = sym("\"");
  return BEGIN_STRING;
}
<RAWSTRING>[^"]+ {
  yylval = str(yytext);
  return STRING_CHUNK;
}
<RAWSTRING>\" {
  BEGIN(INITIAL);
  yylval = sym("\"");
  return END_STRING;
}

'[^\\']' {
  yylval = opi::str("c:#\\" + std::string(yytext + 1, yyleng - 2) + " ");
  return CHAR;
}

'\\.' {
  switch (yytext[2])
  {
    case '\'': yylval = str("c:#\\'"); break;
    case '\\': yylval = str("c:#\\\\"); break;
    case 'a': yylval = str("c:#\\x0007"); break;
    case 'b': yylval = str("c:#\\x0008"); break;
    case 't': yylval = str("c:#\\x0009"); break;
    case 'n': yylval = str("c:#\\x000a"); break;
    case 'v': yylval = str("c:#\\x000b"); break;
    case 'f': yylval = str("c:#\\x000c"); break;
    case 'r': yylval = str("c:#\\x000d"); break;
    case 'e': yylval = str("c:#\\x001b"); break;
    default:
      throw opi::bad_code {std::format("Invalid character literal ({})", yytext)};
  }
  return CHAR;
}

#pragma {
  BEGIN(PRAGMA);
  return PRAGMA_;
}

<PRAGMA>[[:space:]]

<PRAGMA>[-_a-zA-Z0-9]+ {
  yylval = sym(yytext);
  return IDENT;
}

<PRAGMA>\{[^\}]*\} {
  const std::string text {yytext+1, (size_t)yyleng-2};
  yylval = str(text);
  BEGIN(INITIAL);
  return STRING_CHUNK;
}


true {
  yylval = True;
  return BOOLEAN;
}
false {
  yylval = False;
  return BOOLEAN;
}

0[xX][0-9a-fA-F_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 16));
  return INTEGER;
}

0[0-7_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 8));
  return INTEGER;
}

[0-9_]+ {
  if (strcmp(yytext, "_") == 0)
  {
    const size_t oldright = loc_p->left;
    const size_t oldleft = loc_p->right - yyleng;
    loc_p->left = oldleft;
    loc_p->right = oldright;
    REJECT;
  }
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned));
  return INTEGER;
}

\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  return FLOAT;
}

[0-9][0-9_]*\.[0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  return FLOAT;
}

[0-9][0-9_]*[eE][+-]?[0-9][0-9_]* {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  return FLOAT;
}

[_a-z][_a-zA-Z0-9]* {
  if (strcmp(yytext, "_") == 0)
  {
    const size_t oldright = loc_p->left;
    const size_t oldleft = loc_p->right - yyleng;
    loc_p->left = oldleft;
    loc_p->right = oldright;
    REJECT;
  }
  yylval = opi::sym(yytext);
  return IDENT;
}


[A-Z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  return CAPIDENT;
}

{OPCHAR}+ {
  yylval = opi::sym(yytext);

  // Reserved operator-like keywords
  if (yylval == "->")
    return RIGHT_ARROW;
  if (yylval == "=>")
    return FAT_RIGHT_ARROW;
  if (yylval == "::")
    return DOUBLE_COLUMN;
  if (yylval == "=")
    return '=';
  if (yylval == "|")
    return '|';
  if (yylval == ".")
    return '.';
  if (yylval == "?")
    return '?';
  if (yylval == "*")
    return '*';

  // FIXME
  if (yylval == ":")
    return ':';
  if (yylval == "$")
    return '$';

  // Actual operators
  return OPERATOR;
}

"," |
"["|"]" |
"{"|"}" |
"#" |
"_" |
"("|")" {
  yylval = opi::sym(yytext);
  return yytext[0];
}

. {
  opi::error("undefined symbol '{}'", yytext[0]);
  throw std::runtime_error {"Undefined symbol"};
}

%%

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

%%

^"#!".*($|[\r\n])
"//".*($|[\r\n])?
[[:space:]]


require return REQUIRE;

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
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}
<FOREIGN_INLINE>^.+$ {
  yylval = str(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}

foreign return FOREIGN;

assert return ASSERT;

macro return MACRO;

__RETURN return RETURN;

match return MATCH;
in    return IN;

if return IF;
then return THEN;
else return ELSE;

otherwize return OTHERWIZE;

and return AND;
or return OR;
not return NOT;

let return LET;
letrec return LETREC;
where return WHERE;

set return SET;

type return TYPE;

fn return FN;

extern return EXTERN;
as return AS;

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
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return IDENT;
}

"("\s*[-+=!*/%<>:&]+\s*")" {
  std::string ident {yytext + 1, yytext + yyleng - 1};
  while (std::isspace(ident.front()))
    ident.erase(ident.begin());
  while (std::isspace(ident.back()))
    ident.pop_back();
  yylval = sym(ident);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return IDENT;
}


\" {
  BEGIN(STRING);
  return BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}
<STRING>\\['\"\\\nabfnrtve] {
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
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}
<STRING>\\. {
  yylval = str(yytext+1);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}
<STRING>\" {
  BEGIN(INITIAL);
  return END_STRING;
}

r\" {
  BEGIN(RAWSTRING);
  return BEGIN_STRING;
}
<RAWSTRING>[^"]+ {
  yylval = str(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return STRING_CHUNK;
}
<RAWSTRING>\" {
  BEGIN(INITIAL);
  return END_STRING;
}


#pragma {
  BEGIN(PRAGMA);
  return PRAGMA_;
}

<PRAGMA>[[:space:]]

<PRAGMA>[-_a-zA-Z0-9]+ {
  yylval = sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return IDENT;
}

<PRAGMA>\{[^\}]*\} {
  const std::string text {yytext+1, (size_t)yyleng-2};
  yylval = str(text);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  BEGIN(INITIAL);
  return STRING_CHUNK;
}


true {
  yylval = True;
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return BOOLEAN;
}
false {
  yylval = False;
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return BOOLEAN;
}

0[xX][0-9a-fA-F_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 16));
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return INTEGER;
}

0[0-7_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 8));
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
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
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return INTEGER;
}

\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return FLOAT;
}

[0-9][0-9_]*\.[0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return FLOAT;
}

[0-9][0-9_]*[eE][+-]?[0-9][0-9_]* {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
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
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return IDENT;
}


[A-Z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return CAPIDENT;
}

"->" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return RIGHT_ARROW;
}
"=>" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return  FAT_RIGHT_ARROW;
}
"==" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return EQ;
}
"!=" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return NE;
}
"<"  {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return LT;
}
"<=" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return LE;
}
">"  {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return GT;
}
">=" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return GE;
}
"::" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return DOUBLE_COLUMN;
}
"=" |
"+" |
"-" |
"*" |
"/" |
"%" |
"," |
"|" |
"." |
":" |
"&" |
"["|"]" |
"{"|"}" |
"#" |
"_" |
"$" |
"?" |
"("|")" {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, opi::osl::make_location(*loc_p));
  return yytext[0];
}

. {
  opi::error("undefined symbol '{}'", yytext[0]);
  throw std::runtime_error {"Undefined symbol"};
}

%%

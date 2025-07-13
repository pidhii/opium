%{

#include "osl_parser.hpp"
//#include "location.hh"

#include "opium/logging.hpp"
#include "opium/value.hpp"


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


#define YY_DECL extern "C" int yylex(opi::value *yylval_p, range_location *loc_p, yyscan_t yyscanner)
#define yylval (*yylval_p)

extern size_t g_fileoffs_left;
extern size_t g_fileoffs_right;
#define YY_USER_ACTION \
  loc_p->left = loc_p->right; \
  loc_p->right = loc_p->left + yyleng;


opi::source_location
make_location(const range_location &loc);


%}

%option noyywrap
%option reentrant
%option interactive
/*%option c++*/

%x RAWSTRING
%x STRING
%x PRAGMA

%%

^"#!".*($|[\r\n])
"//".*($|[\r\n])
[[:space:]]


require return REQUIRE;

assert return ASSERT;

for return FOR;
do return DO;
break return BREAK;
continue return CONTINUE;
return return RETURN;
yield__ return YIELD;

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

\" {
  BEGIN(STRING);
  return BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
  opi::set_location(yylval, make_location(*loc_p));
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
  opi::set_location(yylval, make_location(*loc_p));
  return STRING_CHUNK;
}
<STRING>\\. {
  yylval = str(yytext+1);
  opi::set_location(yylval, make_location(*loc_p));
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
  opi::set_location(yylval, make_location(*loc_p));
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
  opi::set_location(yylval, make_location(*loc_p));
  return IDENT;
}

<PRAGMA>\{[^\}]*\} {
  const std::string text {yytext+1, (size_t)yyleng-2};
  yylval = str(text);
  opi::set_location(yylval, make_location(*loc_p));
  BEGIN(INITIAL);
  return STRING_CHUNK;
}


true {
  yylval = True;
  opi::set_location(yylval, make_location(*loc_p));
  return BOOLEAN;
}
false {
  yylval = False;
  opi::set_location(yylval, make_location(*loc_p));
  return BOOLEAN;
}

0[xX][0-9a-fA-F_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 16));
  opi::set_location(yylval, make_location(*loc_p));
  return INTEGER;
}

0[0-7_]+ {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned, nullptr, 8));
  opi::set_location(yylval, make_location(*loc_p));
  return INTEGER;
}

[0-9_]+ {
  if (strcmp(yytext, "_") == 0)
    REJECT;
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stoll(cleaned));
  opi::set_location(yylval, make_location(*loc_p));
  return INTEGER;
}

\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, make_location(*loc_p));
  return FLOAT;
}

[0-9][0-9_]*\.[0-9_]*([eE][+-]?[0-9][0-9_]*)? {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, make_location(*loc_p));
  return FLOAT;
}

[0-9][0-9_]*[eE][+-]?[0-9][0-9_]* {
  std::string cleaned = remove_underscores(yytext);
  yylval = num(std::stod(cleaned));
  opi::set_location(yylval, make_location(*loc_p));
  return FLOAT;
}

[_a-z][_a-zA-Z0-9]* {
  if (strcmp(yytext, "_") == 0)
    REJECT;
  yylval = opi::sym(yytext);
  opi::set_location(yylval, make_location(*loc_p));
  return IDENT;
}


[A-Z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  opi::set_location(yylval, make_location(*loc_p));
  return CAPIDENT;
}

"->" return RIGHT_ARROW;
"=>" return FAT_RIGHT_ARROW;

"==" return EQ;
"!=" return NE;
"<"  return LT;
"<=" return LE;
">"  return GT;
">=" return GE;
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
"["|"]" |
"{"|"}" |
"#" |
"_" |
"("|")" {
  return yytext[0];
}

. {
  opi::error("undefined symbol '{}'", yytext[0]);
  throw std::runtime_error {"Undefined symbol"};
}

%%

%{

#include "osl_parser.hpp"

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


#define YY_DECL extern "C" int yylex(opi::value *yylval_p, void *loc_p, yyscan_t yyscanner)
#define yylval (*yylval_p)

%}

%option noyywrap
%option reentrant
%option interactive
/*%option c++*/

%x STRING
%x PRAGMA

%%

^"#!".*($|[\r\n])
"//".*($|[\r\n])
[[:space:]]


require return REQUIRE;

match return MATCH;
in    return IN;

if return IF;
then return THEN;
else return ELSE;

otherwize return OTHERWIZE;

let return LET;
letrec return LETREC;
where return WHERE;

set return SET;

type return TYPE;

\" {
  BEGIN(STRING);
  return BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
  return STRING_CHUNK;
}
<STRING>\\n {
  yylval = str("\n");
  return STRING_CHUNK;
}
<STRING>\\t {
  yylval = str("\t");
  return STRING_CHUNK;
}
<STRING>\\r {
  yylval = str("\r");
  return STRING_CHUNK;
}
<STRING>\\e {
  yylval = str("\e");
  return STRING_CHUNK;
}
<STRING>\\\\ {
  yylval = str("\\");
  return STRING_CHUNK;
}
<STRING>\\\" {
  yylval = str("\"");
  return STRING_CHUNK;
}
<STRING>\\. {
  yylval = str(yytext + 1);
  return STRING_CHUNK;
}
<STRING>\" {
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
  return IDENT;
}

<PRAGMA>\{[^\}]*\} {
  const std::string text {yytext+1, (size_t)yyleng-2};
  yylval = str(text);
  BEGIN(INITIAL);
  return STRING_CHUNK;
}


true { yylval = True; return BOOLEAN; }
false { yylval = False; return BOOLEAN; }

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
  yylval = opi::sym(yytext);
  return IDENT;
}


[A-Z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  return CAPIDENT;
}

"->" return RIGHT_ARROW;

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
"("|")" {
  return yytext[0];
}

. {
  opi::error("undefined symbol '{}'", yytext[0]);
  throw std::runtime_error {"Undefined symbol"};
}

%%

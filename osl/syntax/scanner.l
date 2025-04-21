%{

#include "osl_parser.hpp"

#include "opium/logging.hpp"
#include "opium/value.hpp"




%}

%option noyywrap
/*%option reentrant*/
%option interactive
/*%option c++*/

%x STRING
%x PRAGMA

%%

^"#!".*$
"//".*$
[[:space:]]


match return MATCH;
in    return IN;

otherwize return OTHERWIZE;

let return LET;
letrec return LETREC;
where return WHERE;

type return TYPE;

\" {
  BEGIN(STRING);
  return BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
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
  const std::string_view text {yytext};
  const std::string_view innertext = text.substr(1, text.length()-2);
  yylval = str(std::string(innertext));
  BEGIN(INITIAL);
  return STRING_CHUNK;
}

[0-9_]+ {
  yylval = num(std::stoll(yytext));
  return INTEGER;
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

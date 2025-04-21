%{

#include "osl_parser.hpp"

#include "opium/logging.hpp"
#include "opium/value.hpp"


#define YY_DECL int yylex(opi::value *yylvalp, yy::location *)

#define yylval (*yylvalp)

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


match return yy::parser::token::MATCH;
in    return yy::parser::token::IN;

otherwize return yy::parser::token::OTHERWIZE;

let return yy::parser::token::LET;
letrec return yy::parser::token::LETREC;
where return yy::parser::token::WHERE;

type return yy::parser::token::TYPE;

\" {
  BEGIN(STRING);
  return yy::parser::token::BEGIN_STRING;
}
<STRING>[^\\"]+ {
  yylval = str(yytext);
  return yy::parser::token::STRING_CHUNK;
}
<STRING>\" {
  BEGIN(INITIAL);
  return yy::parser::token::END_STRING;
}


#pragma {
  BEGIN(PRAGMA);
  return yy::parser::token::PRAGMA_;
}

<PRAGMA>[[:space:]]

<PRAGMA>[-_a-zA-Z0-9]+ {
  yylval = sym(yytext);
  return yy::parser::token::IDENT;
}

<PRAGMA>\{[^\}]*\} {
  const std::string_view text {yytext};
  const std::string_view innertext = text.substr(1, text.length()-2);
  yylval = str(std::string(innertext));
  BEGIN(INITIAL);
  return yy::parser::token::STRING_CHUNK;
}

[0-9_]+ {
  yylval = num(std::stoll(yytext));
  return yy::parser::token::INTEGER;
}

[_a-z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  return yy::parser::token::IDENT;
}


[A-Z][_a-zA-Z0-9]* {
  yylval = opi::sym(yytext);
  return yy::parser::token::CAPIDENT;
}

"->" return yy::parser::token::RIGHT_ARROW;

"==" return yy::parser::token::EQ;
"!=" return yy::parser::token::NE;
"<"  return yy::parser::token::LT;
"<=" return yy::parser::token::LE;
">"  return yy::parser::token::GT;
">=" return yy::parser::token::GE;
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

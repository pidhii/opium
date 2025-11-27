#include "macros.hpp"


void
opi::osl::dump_syntax(std::ostream &os, const syntax *syn)
{
  switch (syn->kind)
  {
    case syntax::kind::token:
      if (syn->token.value != nil)
        os << "[tok " << syn->token.type << " " << syn->token.value << "]";
      else if (std::isprint(syn->token.type))
        os << "[tok " << char(syn->token.type) << "]";
      else
        os << "[tok " << syn->token.type << "]";
      break;

    case syntax::kind::sequence:
      os << "[seq ";
      for (const syntax *s : syn->sequence)
        dump_syntax(os, s);
      os << "]";
      break;

    case syntax::kind::group:
      os << "[grp ";
      for (const syntax *s : syn->group.sequence)
        dump_syntax(os, s);
      os << "]";
      break;

    case syntax::kind::parameter:
      os << "[prm " << syn->parameter << "]";
      break;
  }
}


int
opi::osl::entry_token_for(std::string_view parameter_type)
{
  static const std::unordered_map<std::string_view, int> t {
    {"ident", PARSE_IDENT},
    {"expr", PARSE_EXPR},
    {"stmt", PARSE_STMT},
    {"atpat", PARSE_ATPAT},
    {"expat", PARSE_EXPAT},
    {"attype", PARSE_ATTYPE},
    {"extype", PARSE_EXTYPE},
  };
  return t.at(parameter_type);
}

int
opi::osl::token_for(std::string_view parameter_type)
{
  static const std::unordered_map<std::string_view, int> t {
    {"ident", IDENT},
    {"expr", EXPR},
    {"stmt", STMT},
    {"atpat", ATPAT},
    {"expat", EXPAT},
    {"attype", ATTYPE},
    {"extype", EXTYPE},
  };
  return t.at(parameter_type);
}



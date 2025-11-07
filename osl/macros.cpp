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
      for (const syntax *s : syn->group)
        dump_syntax(os, s);
      os << "]";
      break;

    case syntax::kind::parameter:
      os << "[prm " << syn->parameter << "]";
      break;
  }
}
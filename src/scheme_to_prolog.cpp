#include "opium/scheme/scheme_transformations.hpp"


opi::scheme_to_prolog::scheme_to_prolog(type_format_string format)
: m_type_format {format}
{
  // const match ifmatch = {list("if"), list("if", "cond", "then", "else")};
  // append_rule(ifmatch, [this](const auto &ms) {
  //   let
  //   return nil;
  // });
}
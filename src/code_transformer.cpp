#include "opium/code_transformer.hpp"
#include "opium/lisp_parser.hpp"

#include <format>

using namespace std::placeholders;

/**
 * Implementation of the code_transformer class and its derivatives.
 * 
 * This file contains the implementation of the methods defined in code_transformer.hpp,
 * including rule management and expression transformation logic.
 */


void
opi::code_transformer::prepend_rule(const match &matcher,
                                    const transformation &transformer)
{ m_pages.front().emplace_front(matcher, transformer); }


void
opi::code_transformer::append_rule(const match &matcher,
                                   const transformation &transformer)
{ m_pages.front().emplace_back(matcher, transformer); }


opi::value
opi::code_transformer::operator () (value inexpr) const
{
  // Iterate through the syntax table and find the first matching rule
  match_mapping matches;
  for (const auto &[matcher, transformer] : m_pages | std::views::join)
  {
    if (matcher(inexpr, matches))
    {
      const value result = transformer(matches, inexpr);
      source_location location;
      if (lisp_parser::get_location(inexpr, location))
        set_location(result, location);
      return result;
    }
    matches.clear(); // Clean up after unsuccessful match
  }

  // If we reach here, no rule matched the input expression
  throw code_transformation_error {
      std::format("no syntax rule matches the expression: {}", inexpr)};
}

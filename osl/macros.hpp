#pragma once

#include "opium/source_location.hpp"
#include "parse.hpp"
#include "osl_parser.hpp" // FIXME

#include "opium/exceptions.hpp"
#include "opium/format.hpp"
#include "opium/logging.hpp"
#include "opium/stl/vector.hpp"
#include "opium/stl/deque.hpp"
#include <iterator>


namespace opi::osl {

struct syntax;

namespace detail {
  using syntax_group = stl::vector<const syntax*>;
}

struct syntax {
  enum class kind { token, sequence, group, parameter } kind;
  union {
    lexer::token token;
    detail::syntax_group sequence;
    detail::syntax_group group;
    value parameter;
  };
};


int
entry_token_for(std::string_view parameter_type);

int
token_for(std::string_view parameter_type);



static inline bool
is_token(const syntax *syn, int toktype) noexcept
{ return syn->kind == syntax::kind::token and syn->token.type == toktype; }

static inline bool
is_token(const syntax *syn, int toktype, value value) noexcept
{
  return syn->kind == syntax::kind::token and syn->token.type == toktype and
         syn->token.value == value;
}

void
dump_syntax(std::ostream &os, const syntax *syn);


static inline syntax *
make_token(const lexer::token &token)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::token;
  new (&result->token) lexer::token {token};
  return result;
}

template <std::input_iterator Iter, std::sentinel_for<Iter> End>
static inline syntax *
make_syntax_sequence(Iter begin, End end)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::sequence;
  new (&result->sequence) detail::syntax_group {begin, end};
  return result;
}

template <std::input_iterator Iter, std::sentinel_for<Iter> End>
static inline syntax *
make_syntax_group(Iter begin, End end)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::group;
  new (&result->group) detail::syntax_group {begin, end};
  return result;
}

static inline syntax *
make_parameter(value name)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::parameter;
  new (&result->parameter) value {name};
  return result;
}


namespace detail {
enum parse_param_flags : unsigned { type = 1, name = 2, all = 3 };

// <type> ':' <name>
static inline std::pair<value /*type*/, value /*name*/>
parse_pattern_parameter(generic_lexer &lex, unsigned flags = all)
{ // TODO: pass locations to the exceptions
  lexer::token dolar, type, colon, name;
  int toktype;

  // Leading dollar
  if ((toktype = lex.read(dolar)) != '$')
    throw bad_code {
        std::format("invalid macro parameter (expected '$', got '{}'/{})",
                      dolar.value, toktype)};

  // parameter type
  if ((flags & parse_param_flags::type) and (toktype = lex.read(type)) != IDENT)
    throw bad_code {std::format(
        "invalid macro parameter (expected type identifier, got '{}'/{})",
        type.value, toktype)};

  // colon
  if ((flags == parse_param_flags::all) and lex.read(colon) != ':')
    throw bad_code {"invalid macro parameter (expected ':')"};

  // parameter name
  if ((flags & parse_param_flags::name) and lex.read(name) != IDENT)
    throw bad_code {"invalid macro parameter (expected name identifier)"};

  return {type.value, name.value};
}
} // namespace detail


template <typename C, typename K, typename V>
concept associative_container = requires(C c, K k, V v) {
  { c[k] } -> std::convertible_to<V>;
  { c[k] = v };
};


template <associative_container<value, value> ParamDict>
class macro_pattern_parser {
  public:
  macro_pattern_parser(ParamDict &parameter_types)
  : m_parameter_types {parameter_types}
  { }

  template <std::output_iterator<syntax *> Out>
  void
  parse_list(generic_lexer &lex, int listend, Out out);

  syntax *
  parse_syntax(generic_lexer &lex);

  syntax *
  parse_sequence(generic_lexer &lex, int listend)
  {
    stl::vector<const syntax *> syntaxes;
    parse_list(lex, listend, std::back_inserter(syntaxes));
    return make_syntax_sequence(syntaxes.begin(), syntaxes.end());
  }

  private:
  ParamDict &m_parameter_types;
};


template <associative_container<value, value> ParamDict>
syntax *
macro_pattern_parser<ParamDict>::parse_syntax(generic_lexer &lex)
{
  lexer::token token;
  switch (lex.read(token))
  {
    case '$':
    {
      lexer::token nexttoken;
      switch (lex.peek(nexttoken))
      {
        case '(': // syntax group
        {
          lex.read(nexttoken);
          stl::vector<const syntax *> syntaxes;
          parse_list(lex, ')', std::back_inserter(syntaxes));
          return make_syntax_group(syntaxes.begin(), syntaxes.end());
        }

        case IDENT: // parameter
        {
          lex.put(token);
          const auto [type, name] = parse_pattern_parameter(lex, detail::all);
          m_parameter_types[name] = type;
          return make_parameter(name);
        }

        default:
          throw bad_code {"Invalid marco syntax", token.value};
      }
    }

    default:
      return make_token(token);
  }
}

template <associative_container<value, value> ParamDict>
template <std::output_iterator<syntax *> Out>
void
macro_pattern_parser<ParamDict>::parse_list(generic_lexer &lex, int listend, Out out)
{
  while (true)
  {
    const syntax *syn = parse_syntax(lex);

    // Check for end of the list
    if (is_token(syn, listend))
      return;

    // Otherwize, add element to the list
    *out++ = syn;
  }
}



class macro_rule_parser {
  public:
  template <std::output_iterator<syntax *> Out>
  void
  parse_list(generic_lexer &lex, int listend, Out out);

  syntax *
  parse_syntax(generic_lexer &lex);

  syntax *
  parse_sequence(generic_lexer &lex, int listend)
  {
    stl::vector<const syntax *> syntaxes;
    parse_list(lex, listend, std::back_inserter(syntaxes));
    return make_syntax_sequence(syntaxes.begin(), syntaxes.end());
  }
};


inline syntax *
macro_rule_parser::parse_syntax(generic_lexer &lex)
{
  lexer::token token;
  switch (lex.read(token))
  {
    case '$':
    {
      lexer::token nexttoken;
      switch (lex.peek(nexttoken))
      {
        case '(': // syntax group
        {
          lex.read(nexttoken);
          stl::vector<const syntax *> syntaxes;
          parse_list(lex, ')', std::back_inserter(syntaxes));
          return make_syntax_group(syntaxes.begin(), syntaxes.end());
        }

        case IDENT: // parameter
        {
          lex.put(token);
          const auto [_, name] =
              parse_pattern_parameter(lex, detail::name);
          return make_parameter(name);
        }

        default:
          throw bad_code {"Invalid marco syntax", token.value};
      }
    }

    default:
      return make_token(token);
  }
}

template <std::output_iterator<syntax *> Out>
void
macro_rule_parser::parse_list(generic_lexer &lex, int listend, Out out)
{
  while (true)
  {
    const syntax *syn = parse_syntax(lex);

    // Check for end of the list
    if (is_token(syn, listend))
      return;

    // Otherwize, add element to the list
    *out++ = syn;
  }
}



template <associative_container<value, value> ParamTypeDict,
          associative_container<value, value> ParamValDict>
class macro_pattern_matcher {
  public:
  macro_pattern_matcher(const ParamTypeDict &parameter_types,
                        ParamValDict &parameter_vals)
  : m_parameter_types {parameter_types}, m_parameter_vals {parameter_vals}
  { }

  void
  match_syntax(generic_lexer &lex, parser &prs, const syntax *pattern);

  private:
  const ParamTypeDict &m_parameter_types;
  ParamValDict &m_parameter_vals;
};

template <associative_container<value, value> ParamTypeDict,
          associative_container<value, value> ParamValDict>
void
macro_pattern_matcher<ParamTypeDict, ParamValDict>::match_syntax(
    generic_lexer &lex, parser &prs, const syntax *pattern)
{
  switch (pattern->kind)
  {
    case syntax::kind::token:
    {
      lexer::token token;
      if (lex.read(token) != pattern->token.type)
        throw bad_code {"token mismatch", token.value};
      break;
    }

    case syntax::kind::parameter:
    {
      const value name = pattern->parameter;
      const value type = m_parameter_types.at(name);
      m_parameter_vals[name] =
          prs.parse(entry_token_for(sym_name(type)), lex, true);
      debug("matched parameter {}: {}", name, m_parameter_vals[name]);
      break;
    }

    case syntax::kind::sequence:
    case syntax::kind::group:
    {
      for (const syntax *s : pattern->sequence)
        match_syntax(lex, prs, s);
      break;
    }
  }
}


template <associative_container<value, value> ParamTypeDict,
          associative_container<value, value> ParamValDict>
class macro_expander {
  public:
  macro_expander(const ParamTypeDict &parameter_types,
                 const ParamValDict &parameter_vals)
  : m_parameter_types {parameter_types}, m_parameter_vals {parameter_vals}
  { }

  void
  expand(generic_lexer &lex, const syntax *rule)
  {
    stl::deque<lexer::token> result;
    _expand(rule, result);
    for (auto it = result.rbegin(); it != result.rend(); ++it)
      lex.put(*it);
  }

  private:
  void
  _expand(const syntax *rule, stl::deque<lexer::token> &result);

  const ParamTypeDict &m_parameter_types;
  const ParamValDict &m_parameter_vals;
};

template <associative_container<value, value> ParamTypeDict,
          associative_container<value, value> ParamValDict>
void
macro_expander<ParamTypeDict, ParamValDict>::_expand(
    const syntax *rule, stl::deque<lexer::token> &result)
{
  switch (rule->kind)
  {
    case syntax::kind::token:
    {
      result.push_back(rule->token);
      break;
    }

    case syntax::kind::parameter:
    {
      const value name = rule->parameter;
      const value type = m_parameter_types.at(name);
      const value val = m_parameter_vals.at(name);
      source_location location;
      if (get_location(val, location)) { };
      result.emplace_back(location, val, token_for(sym_name(type)));
      break;
    }

    case syntax::kind::sequence:
    case syntax::kind::group:
    {
      for (const syntax *s : rule->sequence)
        _expand(s, result);
      break;
    }
  }
}

} // namespace opi::osl
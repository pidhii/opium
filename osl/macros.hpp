#pragma once

#include "opium/lisp_parser.hpp"
#include "opium/source_location.hpp"
#include "opium/stl/unordered_map.hpp"
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
  enum class kind { token, sequence, group, parameter, op } kind;
  union {
    lexer::token token;
    detail::syntax_group sequence;
    struct { detail::syntax_group sequence; value name; } group;
    value parameter;
    struct { value name; const syntax *arg; } op;
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
make_group(value name, Iter begin, End end)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::group;
  result->group.name = name;
  new (&result->group.sequence) detail::syntax_group {begin, end};
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

static inline syntax *
make_op(value opname, const syntax *oparg)
{
  syntax *result =
      reinterpret_cast<syntax *>(allocate(sizeof(syntax)));
  result->kind = syntax::kind::op;
  new (&result->op) decltype(syntax::op) {opname, oparg};
  return result;
}



namespace detail {
enum parse_param_flags : unsigned { type = 1, name = 2, all = 3 };

// <type> ':' <name>
static std::pair<value /*type*/, value /*name*/>
parse_pattern_parameter(generic_lexer &lex, unsigned flags = all);

} // namespace detail


template <typename C, typename K, typename V>
concept associative_container = requires(C c, K k, V v) {
  { c[k] } -> std::convertible_to<V>;
  { c[k] = v };
};


struct group_type {
  value reptag;
  stl::unordered_map<value, value> types;
};

struct group_value {
  stl::vector<stl::unordered_map<value, value>> values;
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
      if (lex.peek(nexttoken) == '[')
      {
        lex.read(nexttoken);

        group_type *gpat = make<group_type>();
        macro_pattern_parser subparser (gpat->types);
        stl::vector<const syntax*> seq;
        subparser.parse_list(lex, ']', std::back_inserter(seq));
        lexer::token reptag;
        switch (lex.read(reptag))
        {
          case '*':
          case '+':
            gpat->reptag = reptag.value;
            break;
          default:
            throw bad_code {std::format(
                "Invalid macro parameter (expected '*' or '+', got '{}'/{})",
                reptag.value, reptag.type), reptag.location};
        }
        lex.put(token);
        const auto [_, name] = detail::parse_pattern_parameter(lex, detail::name);
        m_parameter_types[name] = ptr(gpat);
        return make_group(name, seq.begin(), seq.end());
      }
      else
      {
        lex.put(token);
        const auto [type, name] = parse_pattern_parameter(lex, detail::all);
        m_parameter_types[name] = type;
        return make_parameter(name);
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
  lexer::token token, nexttoken;
  switch (lex.read(token))
  {
    case '$':
    {
      if (lex.peek(nexttoken) == '[')
      {
        lex.read(nexttoken);
        stl::vector<const syntax*> seq;
        parse_list(lex, ']', std::back_inserter(seq));
        lex.put(token);
        const auto [_, name] = parse_pattern_parameter(lex, detail::name);
        return make_group(name, seq.begin(), seq.end());
      }
      else if (lex.peek(nexttoken) == '#')
      {
        lex.read(nexttoken); // readout the '#'
        lex.put(token);
        const auto [_, name] = detail::parse_pattern_parameter(lex, detail::name);
        return make_op(nexttoken.value, make_parameter(name));
      }
      else
      {
        lex.put(token);
        const auto [_, name] = parse_pattern_parameter(lex, detail::name);
        return make_parameter(name);
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
        throw parse_error {"token mismatch", token.value};
      break;
    }

    case syntax::kind::parameter:
    {
      const value name = pattern->parameter;
      const value type = m_parameter_types.at(name);
      assert(issym(type));
      lexer::token nexttoken;
      m_parameter_vals[name] =
          prs.parse(entry_token_for(sym_name(type)), lex, true);
      break;
    }

    case syntax::kind::group:
    {
      const value name = pattern->group.name;
      const value type = m_parameter_types.at(name);
      assert(isptr(type));
      const group_type *gpat = ptr_val<const group_type*>(type);

      group_value *gval = make<group_value>();
      while (true)
      {
        stl::unordered_map<value, value> values;
        macro_pattern_matcher gmch {gpat->types, values};

        stateful_lexer stlex {lex};
        const stateful_lexer::state state = stlex.current_state();
        const syntax *seq = make_syntax_sequence(pattern->sequence.begin(),
                                                 pattern->sequence.end());
        try
        {
          gmch.match_syntax(stlex, prs, seq);
        }
        catch (const opi::bad_code &exn)
        {
          stlex.recover_state(state);
          break;
        }

        gval->values.emplace_back(std::move(values));
      }

      if (gpat->reptag == "*")
      { /* nothing to check */ }
      else if (gpat->reptag == "+")
      {
        if (gval->values.size() == 0)
          throw parse_error {"+-pattern did not match"};
      }

      m_parameter_vals[name] = ptr(gval);
      break;
    }

    case syntax::kind::sequence:
    {
      for (const syntax *s : pattern->sequence)
        match_syntax(lex, prs, s);
      break;
    }

    case syntax::kind::op:
    {
      error("Did not expect macro operator during syntax match");
      std::terminate();
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
      assert(issym(type));
      const value val = m_parameter_vals.at(name);
      source_location location;
      if (get_location(val, location)) { };
      result.emplace_back(location, val, token_for(sym_name(type)));
      break;
    }

    case syntax::kind::group:
    {
      const value name = rule->group.name;
      const syntax *grule = make_syntax_sequence(rule->group.sequence.begin(),
                                                 rule->group.sequence.end());
      const group_type *type = ptr_val<const group_type*>(m_parameter_types.at(name));
      const group_value *value = ptr_val<const group_value*>(m_parameter_vals.at(name));
      for (const auto &subvals : value->values)
      {
        macro_expander subexp {type->types, subvals};
        subexp._expand(grule, result);
      }
      break;
    }

    case syntax::kind::sequence:
    {
      for (const syntax *s : rule->sequence)
        _expand(s, result);
      break;
    }

    case syntax::kind::op:
    {
      if (rule->op.name == "#")
      {
        assert(rule->op.arg->kind == syntax::kind::parameter);
        const value pname = rule->op.arg->parameter;
        assert(m_parameter_types.at(pname) == "ident");
        const value pval = m_parameter_vals.at(pname);
        const std::string identstr {sym_name(pval)};
        result.emplace_back(*pval->location, opi::str(identstr), EXPR);
        break;
      }
      else
      {
        error("Unknown macro operator: {}", rule->op.name);
        std::terminate();
      }
    }
  }
}

// <type> ':' <name>
static std::pair<value /*type*/, value /*name*/>
detail::parse_pattern_parameter(generic_lexer &lex, unsigned flags)
{ // TODO: pass locations to the exceptions
  lexer::token dolar, type, colon, name;
  int toktype;

  // Leading dollar
  if ((toktype = lex.read(dolar)) != '$')
    throw bad_code {
        std::format("Invalid macro parameter (expected '$', got '{}'/{})",
                      dolar.value, toktype), dolar.location};

  // parameter type
  if ((flags & parse_param_flags::type))
  {
    if (lex.read(type) != IDENT)
    {
      throw bad_code {std::format(
          "Invalid macro parameter (expected macrotype, got '{}'/{})",
          type.value, type.type), type.location};
    }
  }

  // colon
  if ((flags == parse_param_flags::all) and lex.read(colon) != ':')
    throw bad_code {"Invalid macro parameter (expected ':')", colon.location};

  // parameter name
  if ((flags & parse_param_flags::name) and lex.read(name) != IDENT)
    throw bad_code {"Invalid macro parameter (expected name identifier)",
                    name.location};

  return {type.value, name.value};
}

} // namespace opi::osl
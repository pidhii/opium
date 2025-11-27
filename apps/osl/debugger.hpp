#pragma once

#include "opium/logging.hpp"
#include "opium/opium.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/scheme_type_location_map.hpp"


template <size_t I, typename ...Ts>
inline void
_read_values(std::istream &input, std::tuple<Ts...> &out)
{
  if constexpr (I < sizeof...(Ts))
  {
    input >> std::get<I>(out);
    _read_values<I + 1>(input, out);
  }
}

template <typename ...Ts>
inline std::tuple<Ts...>
read_values(std::istream &input)
{
  std::tuple<Ts...> result;
  _read_values<0>(input, result);
  return result;
}

template <typename ...Ts>
inline std::tuple<Ts...>
read_values(std::string_view inputstr)
{
  std::istringstream input {inputstr.data()};
  std::tuple<Ts...> result;
  _read_values<0>(input, result);
  return result;
}


struct command_parser {
  enum result { ok, empty, invalid };

  void
  bind(std::string_view tag, std::initializer_list<std::string_view> binds)
  {
    for (const std::string_view bind : binds)
      commands.emplace(bind, tag);
  }

  result
  operator () (std::string &cmd, std::string &rest) const
  {
    std::string line;
    std::getline(std::cin, line);

    if (line.empty())
      return empty;

    std::istringstream buf {line};

    buf >> cmd;
    const auto it = commands.find(cmd);
    if (it == commands.end())
      return invalid;
    cmd = it->second;

    std::getline(buf, rest);
    return ok;
  }

  std::unordered_map<std::string, std::string> commands;
};



template <std::forward_iterator TraceIter>
struct debugger {
  debugger(const opi::scheme_type_location_map &tlm, TraceIter begin,
           TraceIter end)
  : m_tlm {tlm}, m_trace_point {begin}, m_trace_end {end}
  {
    static_assert(opi::prolog_guide<debugger>);

    m_cmdparser.bind("n", {"next", "n"});
    m_cmdparser.bind("e", {"end", "e"});
    m_cmdparser.bind("b", {"set-break", "b"});
    m_cmdparser.bind("w", {"where", "w"});
    m_cmdparser.bind("t", {"types", "t"});
    m_cmdparser.bind("p", {"print", "p"});
    m_cmdparser.bind("spd", {"set-print-depth", "spd"});
    m_cmdparser.bind("s", {"skip", "s"});
    m_cmdparser.bind("v", {"verbose", "v"});
  }

  void
  _interact(opi::value curexpr, const opi::trace_node *t)
  {
    if (m_last_cmd == "s")
    {
      if (m_trace_point != m_trace_end)
        m_trace_point += 1;
      return;
    }
      
    (std::cout << "> ").flush();
    std::string cmd, cmdargs;
    switch (m_cmdparser(cmd, cmdargs))
    {
      case command_parser::empty:
        if (std::cin.eof())
          return;
        else
          cmd = m_last_cmd;
        break;

      case command_parser::invalid:
        // Warn and retry
        opi::warning("invalid command: '{}'", cmd);
        return _interact(curexpr, t);

      default:
        break;
    }

    if (cmd == "n" or cmd == "s")
    {
      if (m_trace_point != m_trace_end)
        m_trace_point += 1;
    }
    else if (cmd == "e")
      m_trace_point = m_trace_end;
    else if (cmd == "w")
    {
      info("current source location: {} {} {}", t->location.source,
           t->location.start, t->location.end);
      return _interact(curexpr, t);
    }
    else if (cmd == "t")
    {
      try { std::cout << m_tlm.display_location_with_types(t->location); }
      catch (const std::exception &exn) { opi::error("{}", exn.what()); }
      return _interact(curexpr, t);
    }
    else if (cmd == "b")
    {
      try
      {
        auto [source, start, end] = read_values<std::string, int, int>(cmdargs);
        source = std::filesystem::absolute(source).string();
        opi::info("set break-point: {} [{}:{}]", source, start, end);
        m_break_point.emplace(source, start, end);
      }
      catch (const std::exception &exn) { opi::error("{}", exn.what()); }
      return _interact(curexpr, t);
    }
    else if (cmd == "p")
    {
      opi::print(std::cout,
                 opi::reconstruct(curexpr, opi::stringify_unbound_variables),
                 m_print_depth);
      std::cout << std::endl;
      return _interact(curexpr, t);
    }
    else if (cmd == "spd")
    {
      try { std::tie(m_print_depth) = read_values<int>(cmdargs); }
      catch (const std::exception &exn) { opi::error("{}", exn.what()); }
      return _interact(curexpr, t);
    }
    else if (cmd == "v")
    {
      try { std::tie(m_verbose) = read_values<bool>(cmdargs); }
      catch (const std::exception &exn) { opi::error("{}", exn.what()); }
      return _interact(curexpr, t);
    }
    else
    {
      opi::warning("invalid command: '{}'", cmd);
      return _interact(curexpr, t);
    }

    m_last_cmd = cmd;
  }

  bool
  _confirm(const std::string_view prompt)
  {
    if (std::cin.eof())
      return true;

    (std::cout << prompt).flush();
    std::string ans;
    std::getline(std::cin, ans);
    if (ans == "yes" or ans == "y")
      return true;
    else if (ans == "no" or ans == "n")
      return false;
    else
    {
      opi::warning("please answer with 'y(es)?' or 'n(o)?'");
      return _confirm(prompt);
    }
  }

  bool
  operator () (opi::value expr, const opi::trace_node *t)
  {
    if (m_trace_point != m_trace_end and t->location != *m_trace_point)
    {
      if (not t->location.is_file_location())
        return false;

      warning("query diverges\n"
              "know path: {}\n"
              "query path: {}\n",
              display_location(*m_trace_point, 1, "\e[38;5;2;1m"),
              display_location(t->location, 1, "\e[38;5;3;1m"));

      if (_confirm("allow divergence? [y/n]: "))
        m_trace_point = m_trace_end;
      else
        return false;
    }

    if (expr == opi::True or (ispair(expr) and car(expr) == "and"))
    { // Silently advance without asking or notifying
      if (m_trace_point != m_trace_end)
        m_trace_point += 1;
      return true;
    }
    else
    { // Display current expression
      if (t->location.is_file_location())
      {
        try
        {
          std::cout << display_location(t->location, 1, "\e[1m", "\e[2m")
                    << std::endl;
        }
        catch (const std::exception &exn)
        { opi::warning("failed to display location: {}", exn.what()); }

        // Handle break-point
        if (m_break_point.has_value() and m_break_point->contains(t->location))
        {
          m_last_cmd = "";
          std::cout << "break point reached" << std::endl;
          m_break_point = std::nullopt;
        }
      }
      else if (m_verbose)
      {
        opi::print(std::cout,
                   opi::reconstruct(expr, opi::stringify_unbound_variables),
                   m_print_depth);
        std::cout << std::endl;
      }
      else
      {
        if (m_trace_point != m_trace_end)
          m_trace_point += 1;
        return true;
      }
    }

    _interact(expr, t);

    return true;
  }

  private:
  command_parser m_cmdparser;
  bool m_verbose = false;
  int m_print_depth = 4;
  std::string m_last_cmd;
  const opi::scheme_type_location_map &m_tlm;
  TraceIter m_trace_point, m_trace_end;
  std::optional<opi::source_location> m_break_point {
    // "/home/pidhii/sandbox/create/opium/./t/iterators.osl" :3:32 to 3:41
  };
};


static void
interactive_debugger(const opi::scheme_translator &translator,
                     opi::scheme_program &scmprogram)
{
  // Get longest trace
  opi::stl::vector<opi::source_location> maxtrace;
  opi::scan_traces(translator.prolog.query_trace(), [&maxtrace](const auto &tcand) {
    if (tcand.size() > maxtrace.size())
      maxtrace = tcand;
  });

  opi::scheme_type_location_map tlm =
      opi::build_type_location_map(*scmprogram.code_types, *scmprogram.ircode);
  if (scmprogram.type_bindings.has_value())
    tlm.substitute_type_aliases(*scmprogram.type_bindings);

  debugger dbg {tlm, maxtrace.begin(), maxtrace.end()};

  opi::typecheck(scmprogram, translator.prolog, std::ref(dbg));
}

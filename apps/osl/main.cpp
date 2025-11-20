#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "parse.hpp"

#include "opium/logging.hpp"
#include "opium/opium.hpp"
#include "opium/scheme/scheme_emitter.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/utilities/path_resolver.hpp"

#include <boost/program_options.hpp>
#include <cstdlib>
#include <cstring>
#include <filesystem>

#include <dlfcn.h>
#include <initializer_list>
#include <string>
#include <unordered_map>


namespace std {
namespace fs = std::filesystem;
}



static void
load_plugin(const std::fs::path &path)
{
  for (const std::string &prefix : opi::osl::pathes)
  {
    const std::fs::path fullpath = prefix / path;
    if (std::fs::exists(fullpath))
    {
      if (dlopen(fullpath.c_str(), RTLD_NOW) == nullptr)
      {
        opi::error("dlopen {}", dlerror());
        throw std::runtime_error {dlerror()};
      }
      opi::info("Loaded plugin {}", path.c_str());
      return;
    }
  }
  opi::error("failed to find plugin '{}'", path.c_str());
  throw std::runtime_error {std::format("no such plugin '{}'", path.c_str())};
}


static void
tracedump(const opi::prolog &pl, size_t tracelen)
{
  // Get longest trace
  opi::stl::vector<opi::source_location> maxtrace;
  opi::scan_traces(pl.query_trace(), [&maxtrace](const auto &tcand) {
    if (tcand.size() > maxtrace.size())
      maxtrace = tcand;
  });

  opi::stl::vector<opi::source_location> trace = maxtrace;
  const size_t tstart = trace.size() < tracelen ? 0 : trace.size() - tracelen;
  for (size_t t = tstart; t < trace.size(); ++t)
    opi::error("[{:3}] {}", t, display_location(trace[t], 1, "\e[1m", "\e[2m"));
}



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
  debugger(TraceIter begin, TraceIter end)
  : m_trace_point {begin}, m_trace_end {end}
  {
    static_assert(prolog_guide<debugger>);

    m_cmdparser.bind("n", {"next", "n"});
    m_cmdparser.bind("e", {"end", "e"});
    m_cmdparser.bind("b", {"set-break", "b"});
    m_cmdparser.bind("w", {"where", "w"});
    m_cmdparser.bind("p", {"print", "p"});
    m_cmdparser.bind("spd", {"set-print-depth", "spd"});
    m_cmdparser.bind("s", {"skip", "s"});
  }

  void
  _interact(value curexpr)
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
        warning("invalid command: '{}'", cmd);
        return _interact(curexpr);

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
      info("current source location: {} {} {}",
        m_cur_node->location.source,
        m_cur_node->location.start,
        m_cur_node->location.end
      );
      return _interact(curexpr);
    }
    else if (cmd == "b")
    {
      try
      {
        const auto [source, start, end] =
            read_values<std::string, int, int>(cmdargs);
        m_break_point.emplace(source, start, end);
      }
      catch (const std::exception &exn) { error("{}", exn.what()); }
      return _interact(curexpr);
    }
    else if (cmd == "p")
    {
      opi::print(std::cout,
                 opi::reconstruct(curexpr, opi::stringify_unbound_variables),
                 m_print_depth);
      std::cout << std::endl;
      return _interact(curexpr);
    }
    else if (cmd == "spd")
    {
      try { std::tie(m_print_depth) = read_values<int>(cmdargs); }
      catch (const std::exception &exn) { error("{}", exn.what()); }
      return _interact(curexpr);
    }
    else
    {
      warning("invalid command: '{}'", cmd);
      return _interact(curexpr);
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
      warning("please answer with 'y(es)?' or 'n(o)?'");
      return _confirm(prompt);
    }
  }

  bool
  operator () (opi::value expr, const opi::trace_node *t)
  {
    m_cur_node = t;
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

    if (expr == True or (ispair(expr) and car(expr) == "and"))
    { // Silently advance without asking or notifying
      if (m_trace_point != m_trace_end)
        m_trace_point += 1;
      return true;
    }
    else
    { // Display current expression
      if (t->location.is_file_location())
      {
        std::cout << display_location(t->location, 1, "\e[1m", "\e[2m") << std::endl;
        // Handle break-point
        if (m_break_point.has_value() and t->location.contains(*m_break_point))
        {
          m_last_cmd = "";
          std::cout << "break point reached" << std::endl;
        }
      }
      else
      {
        opi::print(std::cout, opi::reconstruct(expr, stringify_unbound_variables),
                  m_print_depth);
        std::cout << std::endl;
      }
    }

    _interact(expr);

    return true;
  }

  private:
  command_parser m_cmdparser;
  int m_print_depth = 4;
  const opi::trace_node *m_cur_node;
  std::string m_last_cmd;
  TraceIter m_trace_point, m_trace_end;
  std::optional<opi::source_location> m_break_point {
    // "/home/pidhii/sandbox/create/opium/./t/iterators.osl" :3:32 to 3:41
  };
};


static void
interactive_debugger(const opi::scheme_translator &translator,
                     opi::value opiprogram)
{
  // Get longest trace
  opi::stl::vector<opi::source_location> maxtrace;
  opi::scan_traces(translator.prolog.query_trace(), [&maxtrace](const auto &tcand) {
    if (tcand.size() > maxtrace.size())
      maxtrace = tcand;
  });

  debugger dbg {maxtrace.begin(), maxtrace.end()};
  generate_scheme(translator, opiprogram, "/dev/null", std::ref(dbg));
}


static void
write_scheme_script(std::ostream &os, opi::value script)
{
  for (const opi::value expr : range(script))
    os << opi::strip_escape_sequences(pprint_scm(expr)) << "\n\n";
}




int
main(int argc, char **argv)
{
  namespace po = boost::program_options;
  using namespace opi;

  std::string verbosity {loglevel_name(loglevel::info)};
  std::vector<std::string> flags;
  std::vector<std::fs::path> load;
  std::string opath = "out.scm";
  std::vector<std::string> extra_oslpathes;
  size_t tracelen = 10;

  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::fs::path>()->required(), "input file to process")
    ("verbosity,v", po::value(&verbosity)->implicit_value("debug"), "verbosity")
    ("flag,f", po::value(&flags), "flags")
    ("output,o", po::value(&opath), "write Scheme script to the specified file")
    ("oslpath", po::value(&extra_oslpathes),
     "specify additional directory for filename resolution")
    ("trace-length", po::value(&tracelen), "maximal back-trace length")
    ("opi", "stop after translation to opium DSL")
    ("no-builtins", "don't inject `require \"builtins\"` into the input");

  po::positional_options_description posdesc;
  posdesc.add("input-file", 1);

  po::variables_map varmap;
  try
  {
    auto parsedopts = po::command_line_parser(argc, argv)
                          .options(desc)
                          .positional(posdesc)
                          .run();
    po::store(parsedopts, varmap);
    po::notify(varmap);
  }
  catch (const po::error &e)
  {
    error("{}", e.what());
    std::cerr << desc << std::endl;
    return EXIT_FAILURE;
  }

  // Print help
  if (varmap.contains("help"))
  {
    std::cout << "Usage: " << argv[0] << " [options] [input-file]" << std::endl;
    std::cout << desc << std::endl;
    return EXIT_SUCCESS;
  }

  // Set global log-level
  loglevel = parse_loglevel(verbosity);

  // Set global flags
  for (const std::string &flag : flags)
    global_flags.emplace(flag);

  // Add fragments of OSLPATH supplied in command-line
  for (const std::string &path : extra_oslpathes)
    osl::pathes.emplace_back(path);

  // Import OSLPATH
  if (const char *oslpath = std::getenv("OSLPATH"))
  {
    std::string buf = oslpath;
    char *token = strtok(buf.data(), ":");
    while (token != nullptr)
    {
      osl::pathes.emplace_back(token);
      token = strtok(nullptr, ":");
    }
  }

  // Update LD_LIBRARY_PATH
  for (const std::string &path : osl::pathes)
  {
    const char *old_value = getenv("LD_LIBRARY_PATH");
    const std::string new_value = old_value ? std::string(old_value) + ":" + path
                                            : path;
    setenv("LD_LIBRARY_PATH", new_value.c_str(), true);
  }
  info("Updated LD_LIBRARY_PATH: {}", getenv("LD_LIBRARY_PATH"));

  // Update LIBRARY_PATH
  for (const std::string &path : osl::pathes)
  {
    const char *old_value = getenv("LIBRARY_PATH");
    const std::string new_value = old_value ? std::string(old_value) + ":" + path
                                            : path;
    setenv("LIBRARY_PATH", new_value.c_str(), true);
  }
  info("Updated LIBRARY_PATH: {}", getenv("LIBRARY_PATH"));

  // Load plugins
  load_plugin("parameterize.plugin");


  std::string inputpath;
  if (varmap.contains("input-file"))
    inputpath = varmap["input-file"].as<std::fs::path>();

  // Parse program from OSL into OPI
  opi::value opiprogram;
  try
  {
    opi::osl::program_sources program;
    opi::osl::tree_parser parser {program};

    execution_timer parse_timer {"parsing OSL"};
    program.append("syntax-requirements", osl::syntax_requirements());
    if (not varmap.contains("no-builtins"))
    {
      const std::filesystem::path builtinspath = opi::resolve_path(
          "builtins.osl", osl::pathes.begin(), osl::pathes.end());
      parser.load_file(builtinspath);
    }
    parser.load_file(inputpath);
    parse_timer.stop();

    opiprogram = program.build_program();

    if (varmap.contains("opi"))
    {
      std::ofstream out {opath};
      write_scheme_script(out, opiprogram);
      return EXIT_SUCCESS;
    }
  }
  catch (const opi::bad_code &exn)
  {
    opi::error("{}", exn.display());
    return EXIT_FAILURE;
  }


  scheme_translator translator;
  // Configure preprocessor
  translator.preprocessor.set_norename_prefix("norename#");

  // Configure Prolog interpreter
  for (const std::string &prefix : osl::pathes)
    translator.prolog.add_path_prefix(prefix);
  opi::apply_prolog_pragmas(opiprogram, translator.prolog); // FIXME

  try
  {
    // Compile OPI program into scheme
    generate_scheme(translator, opiprogram, opath);
  }
  catch (const opi::ambiguous_type_error &exn)
  {
    opi::error("{}", exn.what());
    return EXIT_FAILURE;
  }
  catch (const opi::typecheck_failure &exn)
  {
    opi::error("{}", exn.what());
    inputpath = varmap["input-file"].as<std::fs::path>();
    std::ifstream inputfile {inputpath};
    exn.tlm.display_source_with_types(inputpath, inputfile, std::cerr);

    tracedump(translator.prolog, tracelen);
    if (isatty(STDIN_FILENO))
      interactive_debugger(translator, opiprogram);

    return EXIT_FAILURE;
  }
  catch (const opi::bad_code &exn)
  {
    opi::error("{}", exn.display());
    tracedump(translator.prolog, tracelen);
    return EXIT_FAILURE;
  }

  execution_timer::report_global_stats();
}

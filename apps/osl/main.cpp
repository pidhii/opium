#include "parse.hpp"
#include "./debugger.hpp"

#include "opium/logging.hpp"
#include "opium/opium.hpp"
#include "opium/prolog.hpp"
#include "opium/scheme/translator/exceptions.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/utilities/path_resolver.hpp"

#include <boost/program_options.hpp>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>

#include <dlfcn.h>
#include <string>


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




static void
write_scheme_script(std::ostream &os, opi::value script)
{
  for (const opi::value expr : range(script))
    os << opi::pprint_scm(raw_printer, expr) << "\n\n";
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
    ("no-builtins", "don't inject `require \"builtins\"` into the input")
    ("debugger,d", "run interactive debugger upon translation");

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

  // Get input path
  const std::string inputpath = varmap["input-file"].as<std::fs::path>();

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

  // Pre-configure translator:
  scheme_translator translator;
  // o pass oslpathes to the Prolog interpreter
  for (const std::string &prefix : osl::pathes)
    translator.prolog.add_path_prefix(prefix);

  // Parse program from OSL into OPI
  opi::value opicode;
  try
  {
    opi::osl::program_sources codesources;
    opi::osl::program_parser parser {codesources, translator};

    execution_timer parse_timer {"parsing OSL"};
    if (not varmap.contains("no-builtins"))
    {
      const std::filesystem::path builtinspath = opi::resolve_path(
          "builtins.osl", osl::pathes.begin(), osl::pathes.end());
      parser.load_file(builtinspath);
    }
    parser.load_file(inputpath);
    parse_timer.stop();

    opicode = codesources.build_program();

    if (varmap.contains("opi"))
    {
      std::ofstream out {opath};
      write_scheme_script(out, opicode);
      return EXIT_SUCCESS;
    }
  }
  catch (const opi::bad_code &exn)
  {
    opi::error("{}", exn.display());
    return EXIT_FAILURE;
  }

  opi::scheme_program scmprogram;
  try
  {
    // Compile OPI program into scheme
    translate_to_scheme(translator, opicode, scmprogram);

    // Write Scheme script to a file
    if (std::ofstream ofile {opath})
      write_scheme_script(ofile, *scmprogram.scheme_script);

    // (optional) Run debugger
    if (varmap.contains("debugger"))
      interactive_debugger(translator, scmprogram);
  }
  catch (const opi::ambiguous_type_error &exn)
  {
    opi::error("{}", exn.what());
    return EXIT_FAILURE;
  }
  catch (const opi::typecheck_failure &exn)
  {
    opi::error("{}", exn.what());

    tracedump(translator.prolog, tracelen);
    if (isatty(STDIN_FILENO))
      interactive_debugger(translator, scmprogram);

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

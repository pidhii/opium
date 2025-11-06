#include "opium/logging.hpp"
#include "opium/scheme/scheme_emitter.hpp"
#include "opium/scheme/scheme_type_system.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/opium.hpp"

#include <boost/program_options.hpp>
#include <cstdlib>
#include <filesystem>
#include <cstdlib>
#include <cstring>
#include <algorithm>

#include <dlfcn.h>


namespace std {
namespace fs = std::filesystem;
}

namespace opi::osl {

opi::value
parse(const std::string &source, std::FILE *file, bool is_root = true);

extern std::vector<std::string> pathes;

} // namespace opi::osl



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
    ("input-file", po::value<std::fs::path>(), "input file to process")
    ("verbosity,v", po::value(&verbosity)->implicit_value("debug"), "verbosity")
    ("flag,f", po::value(&flags), "flags")
    ("output,o", po::value(&opath), "write Scheme script to the specified file")
    ("oslpath", po::value(&extra_oslpathes),
     "specify additional directory for filename resolution")
    ("trace-length", po::value(&tracelen), "maximal back-trace length")
    ("opi", "stop after translation to opium DSL");

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


  FILE *input;
  std::string inputpath;
  if (varmap.contains("input-file"))
  {
    inputpath = varmap["input-file"].as<std::fs::path>();
    if (not(input = fopen(inputpath.c_str(), "r")))
    {
      error("failed to open file {} for reading", inputpath);
      return EXIT_FAILURE;
    }
  }
  else
  {
    inputpath = "<stream>";
    input = stdin;
  }

  prolog_repl pl;
  try
  {
    execution_timer parse_timer {"parsing OSL"};
    const value result = osl::parse(inputpath, input);
    parse_timer.stop();

    if (varmap.contains("opi"))
    {
      write_scheme_script(std::cout, result);
      return EXIT_SUCCESS;
    }

    // Share path prefixes with Prolog interpreter
    for (const std::string &prefix : osl::pathes)
      pl.add_path_prefix(prefix);

    generate_scheme(result, pl, opath);
  }
  catch (const opi::ambiguous_type_error &exn)
  {
    opi::error("{}", exn.what());
    return EXIT_FAILURE;
  }
  catch (const opi::typecheck_failure &exn)
  {
    opi::error("{}", exn.what());
    if (varmap.contains("input-file"))
    {
      inputpath = varmap["input-file"].as<std::fs::path>();
      std::ifstream inputfile {inputpath};
      exn.tlm.display_source_with_types(inputpath, inputfile, std::cerr);
    }
    tracedump(pl, tracelen);
    return EXIT_FAILURE;
  }
  catch (const opi::bad_code &exn)
  {
    opi::error("{}", exn.display());
    tracedump(pl, tracelen);
    return EXIT_FAILURE;
  }
  
  execution_timer::report_global_stats();
}

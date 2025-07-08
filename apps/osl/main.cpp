#include "opium/logging.hpp"
#include "opium/source_location.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/pretty_print.hpp"
#include "opium/opium.hpp"

#include <boost/program_options.hpp>
#include <cstdlib>
#include <regex>
#include <filesystem>
#include <cstdlib>
#include <cstring>


namespace std {
namespace fs = std::filesystem;
}

namespace opi::osl {

opi::value
parse(const std::string &source, std::FILE *file, bool is_root = true);

extern std::vector<std::string> pathes;

} // namespace opi::osl


/**
 * Strip ANSI escape sequences from a string
 * 
 * \param input The input string containing escape sequences
 * \return The string with escape sequences removed
 */
std::string
strip_escape_sequences(const std::string &input)
{
  // Regex to match escape sequences starting with '\e' and ending with 'm'
  static const std::regex escape_seq_regex("\\\e\\[[^m]*m");
  return std::regex_replace(input, escape_seq_regex, "");
}


void
write_output(std::ostream &out, opi::value code)
{
  std::ostringstream buf;
  for (const opi::value expr : range(code))
  {
    opi::pprint_scm(buf, expr);
    buf << "\n\n";
  }
  out << strip_escape_sequences(buf.str());
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

  po::options_description desc {"Allowed options"};
  desc.add_options()
    ("help", "produce help message")
    ("input-file", po::value<std::fs::path>(), "input file to process")
    ("verbosity,v", po::value<std::string>(&verbosity)->implicit_value("debug"), "verbosity")
    ("flag,f", po::value<std::vector<std::string>>(&flags), "flags")
    ("output,o", po::value<std::string>(&opath), "write Scheme script to the specified file");

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

  execution_timer parse_timer {"parsing OSL"};
  const value result = osl::parse(inputpath, input);
  parse_timer.stop();

  prolog_repl pl;
  try {
    generate_scheme(result, pl, opath);
  }
  catch (...)
  {
    opi::source_location location;
    if (pl.blame_list().get_blame(inputpath, location))
      error("blame {}", display_location(location));
  }
  
  execution_timer::report_global_stats();
}

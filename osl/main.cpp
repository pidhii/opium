#include "opium/logging.hpp"
#include "opium/utilities/execution_timer.hpp"
#include "opium/pretty_print.hpp"

#include <regex>


namespace opi::osl {

opi::value
parse(std::FILE *file);

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
  using namespace opi;

  loglevel = loglevel::debug;

  FILE *input;
  if (argc >= 2)
  {
    if (not (input = fopen(argv[1], "r")))
    {
      error("failed to open file {} for reading", argv[1]);
      return EXIT_FAILURE;
    }
  }
  else
    input = stdin;


  execution_timer parse_timer {"parsing"};
  const value result = osl::parse(input);
  parse_timer.stop();

  execution_timer write_timer {"writing opium scheme"};
  write_output(std::cout, result);
  write_timer.stop();
}
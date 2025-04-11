#pragma once

#include "opium/code_transformer.hpp"
#include "opium/value.hpp"


namespace opi {

class pretty_printer {
  public:
  pretty_printer(const code_transformer &formatter);

  static value
  format_block(bool keep_first, int extra_indent, value x)
  { return list("_FormatBlock", keep_first ? True : False, extra_indent, x); }

  void
  print(std::ostream &os, value x, int indent = 0);

  void
  operator () (std::ostream &os, value x, int indent = 0)
  { print(os, x, indent); }

  std::string
  operator () (value x, int indent = 0) noexcept
  {
    std::ostringstream buf;
    print(buf, x, indent);
    return buf.str();
  }

  private:
  struct _block_format {
    bool keep_first;
    int extra_indent;
  };

  void
  _print_block(std::ostream &os, opi::value stmt, int indent,
               const _block_format &fmt);

  const code_transformer &m_formatter;
}; // class opi::pretty_print


struct scheme_formatter: public code_transformer {
  scheme_formatter();
};

template <typename ...Args>
inline auto
pprint_scm(Args&& ...args)
{
  scheme_formatter scmfmt;
  pretty_printer pprint {scmfmt};
  return pprint(std::forward<Args>(args)...);

}

struct prolog_indenter: public code_transformer {
  prolog_indenter();
};

template <typename ...Args>
inline auto
pprint_pl(Args&& ...args)
{
  prolog_cleaner cleaner;
  prolog_indenter indenter;

  code_transformer fmt;
  fmt.append_rule({nil, "x"}, [&](const auto &ms) {
    return indenter(cleaner(ms.at("x")));
  });
  pretty_printer pprint {fmt};
  return pprint(std::forward<Args>(args)...);
}


} // namespace opi
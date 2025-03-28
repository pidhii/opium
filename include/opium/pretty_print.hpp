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

struct prolog_formatter: public code_transformer {
  prolog_formatter();
};

} // namespace opi
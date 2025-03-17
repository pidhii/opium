#pragma once

#include "opium/prolog.hpp"
#include "opium/value.hpp"

#include <stdexcept>


namespace opi {

class prolog_repl: public prolog {
  public:
  struct error: public std::runtime_error {
    using std::runtime_error::runtime_error;
  };

  void
  operator << (value expr);

  private:
  void
  _query(value expr);
}; // class opi::prolog_repl

} // namespace opi

#include "opium/prolog.hpp"
#include "opium/value.hpp"
#include <regex.h>


const opi::predicate&
opi::prolog::add_predicate(opi::value sig, opi::value body)
{
  return m_db
      .emplace(std::piecewise_construct,
               std::forward_as_tuple(car(sig)->sym.data),
               std::forward_as_tuple(sig, body))
      ->second;
}

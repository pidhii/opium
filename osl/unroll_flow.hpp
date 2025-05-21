#pragma once

#include "opium/value.hpp"


namespace opi::osl {

enum class flow {
  keep,
  branch
};


struct flow_unroller {
  virtual std::pair<flow, value>
  make_break(value args) const = 0;

  virtual std::pair<flow, value>
  make_continue(value args) const = 0;

  virtual std::pair<flow, value>
  make_return(value args) const = 0;

  std::pair<flow, value>
  unroll(value expr, value next) const;

  std::pair<flow, value>
  unroll_block(value exprs, value next, value acc) const;

}; // struct opi::osl::flow_unroller

} // namespace opi::osl
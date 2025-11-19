#pragma once

#include "opium/value.hpp"


namespace opi::osl {

enum class flow {
  keep,
  branch
};


struct flow_unroller {
  flow_unroller(std::string_view return_name): m_return_name {return_name} { }

  virtual std::pair<flow, value>
  make_return(value args) const = 0;

  std::pair<flow, value>
  unroll(value expr, value next) const;

  std::pair<flow, value>
  unroll_block(value exprs, value next, value acc) const;

  private:
  const std::string m_return_name;
}; // struct opi::osl::flow_unroller

} // namespace opi::osl
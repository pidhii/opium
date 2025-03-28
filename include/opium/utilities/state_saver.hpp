#pragma once

#include <tuple>


namespace opi::utl {

template <typename ...T>
struct state_saver {
  state_saver(T &...refs): m_save {refs...}, m_refs {refs...} {}

  ~state_saver()
  { m_refs = m_save; }

  std::tuple<T...> m_save;
  std::tuple<T&...> m_refs;
}; // struct state_saver

} // namespace opi::utl
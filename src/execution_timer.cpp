/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "opium/utilities/execution_timer.hpp"
#include "opium/logging.hpp"


namespace opi {

execution_timer::execution_timer(std::string_view name, bool auto_start)
: m_name {name},
  m_running(false),
  m_total_duration(std::chrono::nanoseconds::zero())
{
  if (auto_start)
    start();
}

execution_timer::~execution_timer()
{
  if (m_running)
    stop();
}

void
execution_timer::start()
{
  if (not m_running)
  {
    m_start_time = std::chrono::high_resolution_clock::now();
    m_running = true;
  }
}

void
execution_timer::stop()
{
  if (m_running)
  {
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = end_time - m_start_time;
    m_total_duration += duration;
    m_running = false;
    _report();
  }
}

void
execution_timer::reset()
{
  m_running = false;
  m_total_duration = std::chrono::nanoseconds::zero();
}

void
execution_timer::_report() const
{
  double ms = elapsed<std::chrono::milliseconds>().count();

  // Format time in appropriate units
  if (ms < 1.0)
    opi::info("\e[1m{}\e[0m completed in {:.3f} μs", m_name, ms * 1000.0);
  else if (ms < 1000.0)
    opi::info("\e[1m{}\e[0m completed in {:.3f} ms", m_name, ms);
  else
    opi::info("\e[1m{}\e[0m completed in {:.3f} s", m_name, ms / 1000.0);
}

} // namespace opi

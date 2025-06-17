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

#include <unordered_map>


namespace opi {

static 
std::unordered_map<std::string, std::chrono::nanoseconds> g_total_duration;
static
std::unordered_map<std::string, std::chrono::nanoseconds> g_max_duration;

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
    // _report();

    // Update total duration
    g_total_duration[m_name] += duration;
    
    // Update max duration if this execution took longer
    if (not g_max_duration.contains(m_name) ||
        duration > g_max_duration[m_name])
      g_max_duration[m_name] = duration;
  }
}

void
execution_timer::reset()
{
  m_running = false;
  m_total_duration = std::chrono::nanoseconds::zero();
}

void
execution_timer::report() const
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

void
execution_timer::report_global_stats()
{
  std::map<std::chrono::nanoseconds, std::string, std::greater<>> entries;

  for (const auto &[name, duration] : g_total_duration)
  {
    const double total_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();

    const double max_ms =
        g_max_duration.contains(name)
            ? std::chrono::duration_cast<std::chrono::milliseconds>(
                  g_max_duration[name])
                  .count()
            : 0.0;

    // Format total time
    std::string total_time;
    if (total_ms < 1.0)
      total_time = std::format("{:.3f} μs", total_ms * 1000.0);
    else if (total_ms < 1000.0)
      total_time = std::format("{:.3f} ms", total_ms);
    else
      total_time = std::format("{:.3f} s", total_ms / 1000.0);
    
    // Format max time
    std::string max_time;
    if (max_ms < 1.0)
      max_time = std::format("{:.3f} μs", max_ms * 1000.0);
    else if (max_ms < 1000.0)
      max_time = std::format("{:.3f} ms", max_ms);
    else
      max_time = std::format("{:.3f} s", max_ms / 1000.0);
    
    // Report both total and max duration
    const std::string text = std::format("\e[1m{:20}\e[0m - total: {}, max: {}",
                                         name, total_time, max_time);
    entries.emplace(duration, text);
  }

  for (const auto &[_, text] : entries)
    info("{}", text);
}

} // namespace opi

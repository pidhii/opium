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


#pragma once

#include <chrono>
#include <string_view>

namespace opi {

/**
 * Utility class to measure execution time of code blocks
 * 
 * This class can be used to measure the execution time of code fragments.
 * It starts timing when constructed and stops when destroyed, or timing
 * can be controlled manually with start() and stop() methods.
 * 
 * Usage example:
 * {
 *     execution_timer timer("Operation name");
 *     // Code to measure
 * } // Timer automatically reports when destroyed
 * 
 * Or with manual control:
 * execution_timer timer("Operation name", false); // Don't start automatically
 * timer.start();
 * // Code to measure
 * timer.stop(); // Stops and reports
 */
class execution_timer {
  public:
  /**
     * Construct a new Execution Timer object
     * 
     * \param name Name of the operation being timed
     * \param auto_start Whether to start timing immediately (default: true)
     */
  explicit execution_timer(std::string_view name, bool auto_start = true);

  /**
   * Destroy the Execution Timer object and report if still running
   */
  ~execution_timer();

  static void
  report_global_stats();

  /**
   * Start the timer
   */
  void
  start();

  /**
   * Stop the timer and report elapsed time
   */
  void
  stop();

  /**
   * @brief Reset the timer
   */
  void
  reset();

  /**
   * @brief Get the elapsed time
   * 
   * @return double Elapsed time
   */
  template <typename Duration>
  Duration
  elapsed() const
  {
    return std::chrono::duration_cast<Duration>(m_total_duration);
  }

  void
  report() const;

  private:
  std::string m_name;
  bool m_running;
  std::chrono::time_point<std::chrono::high_resolution_clock> m_start_time;
  std::chrono::nanoseconds m_total_duration;
};

} // namespace opi

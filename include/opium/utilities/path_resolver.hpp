#pragma once

#include <filesystem>


namespace opi {

template <typename BeginIter, typename EndIter>
std::filesystem::path
resolve_path(const std::filesystem::path &filename, BeginIter begin,
             EndIter end)
{
  for (; begin != end; ++begin)
  {
    const std::filesystem::path prefix = *begin;
    const std::filesystem::path fullpath =
        prefix == "" ? filename : prefix / filename;
    if (std::filesystem::exists(fullpath))
      return std::filesystem::absolute(fullpath);
  }

  throw std::runtime_error {"Failed to resolve path"};
}

} // namespace opi
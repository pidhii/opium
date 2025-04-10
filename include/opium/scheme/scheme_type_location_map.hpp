#pragma once

#include "opium/scheme/scheme_emitter_context.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/source_location.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/value.hpp"
#include <functional>


namespace opi {

// Custom hash function for source_location
struct source_location_hash {
  size_t
  operator () (const source_location &loc) const
  {
    size_t h1 = std::hash<std::string> {}(loc.source);
    size_t h2 = std::hash<size_t> {}(loc.start);
    size_t h3 = std::hash<size_t> {}(loc.end);
    return h1 ^ (h2 << 1) ^ (h3 << 2);
  }
};


// Custom equality function for source_location
struct source_location_equal {
  bool
  operator () (const source_location &lhs, const source_location &rhs) const
  {
    return lhs.source == rhs.source && lhs.start == rhs.start &&
           lhs.end == rhs.end;
  }
};


/**
 * A map from source locations to types
 * 
 * This class represents a mapping from source locations to their corresponding types.
 * It is used to track type information for code elements based on their location in the source.
 */
class scheme_type_location_map {
  public:
  /**
   * Create a new type location map
   */
  scheme_type_location_map() = default;

  /**
   * Add a mapping from a source location to a type
   * 
   * \param loc Source location
   * \param type Type associated with the location
   */
  void
  add(const source_location &loc, value type);

  /**
   * Get the type associated with a source location
   * 
   * \param loc Source location
   * \param[out] type Type associated with the location if found
   * \return True if a type was found for the location
   */
  [[nodiscard]] bool
  get_type(const source_location &loc, value &type) const;

  /**
   * Check if a source location has an associated type
   * 
   * \param loc Source location
   * \return True if the location has an associated type
   */
  [[nodiscard]] bool
  has_type(const source_location &loc) const;

  /**
   * Get the number of mappings in the map
   * 
   * \return Number of mappings
   */
  [[nodiscard]] size_t
  size() const;

  /**
   * Check if the map is empty
   * 
   * \return True if the map is empty
   */
  [[nodiscard]] bool
  empty() const;

  /**
   * Clear all mappings from the map
   */
  void
  clear();

  /**
   * Substitute type aliases with referenced values
   */
  template <typename QueryResult>
  void
  substitute_type_aliases(const QueryResult &query_result)
  {
    for (auto &[_, type] : m_map)
    {
      const auto it = query_result.find(type);
      if (it != query_result.end())
      {
        assert(std::next(it->second.begin(), 1) == it->second.end());
        type = *it->second.begin();
      }
    }
  }

  /**
   * Display a source file with type annotations
   * 
   * This function reads a source file and displays it with type annotations in the format
   * `<expr>:<type>`, where `:type` is written in a different color.
   * 
   * \param type_style ANSI style string for type annotations (default: green)
   */
  void
  display_source_with_types(std::istream &in, std::ostream &out,
                            std::string_view type_style = "\e[2m") const;

  private:
  opi::stl::unordered_map<source_location, value, source_location_hash,
                          source_location_equal>
      m_map;
};


/**
 * Build a map of source locations to types by scanning preprocessed code
 * 
 * This function scans the preprocessed code and builds a map of source locations to types
 * by combining information from the prolog_emitter and the source locations in the code.
 * 
 * \param prolog_emitter The prolog emitter containing type information
 * \param ppcode The preprocessed code to scan
 * \return A map of source locations to types
 */
scheme_type_location_map
build_type_location_map(const scheme_to_prolog &prolog_emitter, value ppcode);

} // namespace opi

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


#include "opium/match.hpp"
#include "opium/stl/unordered_map.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>


// Test fixture for match tests
class MatchTest: public testing::Test {
  protected:
  void
  SetUp() override
  {
    // Common setup code
  }

  void
  TearDown() override
  {
    // Common cleanup code
  }
};

// Test basic pattern matching with literals
TEST_F(MatchTest, BasicLiteralMatching)
{
  // Create a list of literals - symbols that must match exactly in the pattern
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"), opi::sym("c"));

  // Create a pattern with a literal symbol
  opi::value pattern = opi::sym("a");

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Test matching with literals - 'a' must match exactly with 'a' since it's in the literals list
  EXPECT_TRUE(matcher(opi::sym("a")));
  EXPECT_FALSE(matcher(opi::sym("d")));

  // Test with a different pattern
  opi::match matcher2(literals, opi::sym("b"));
  EXPECT_TRUE(matcher2(opi::sym("b")));
  EXPECT_FALSE(matcher2(opi::sym("x")));
}

// Test pattern matching with variables (symbols not in literals list)
TEST_F(MatchTest, VariableMatching)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with a variable - 'X' is not in literals, so it's treated as a variable
  opi::value pattern = opi::sym("X");

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with a variable - X can match any value since it's not in literals
  EXPECT_TRUE(matcher(opi::sym("c"), bindings));

  // Check that the variable was bound correctly
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), opi::sym("c")));

  // Test that literals are not treated as variables
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  opi::match matcher2(literals, opi::sym("a"));
  
  // 'a' is in literals, so it must match exactly with 'a'
  EXPECT_TRUE(matcher2(opi::sym("a"), bindings2));
  EXPECT_FALSE(matcher2(opi::sym("c"), bindings2));
  
  // No bindings should be created for literals
  EXPECT_TRUE(bindings2.empty());
}

// Test pattern matching with pairs/lists
TEST_F(MatchTest, PairMatching)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with a pair - 'a' is a literal, 'X' is a variable
  opi::value pattern = opi::pair(opi::sym("a"), opi::sym("X"));

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with a pair - 'a' must match exactly, 'X' can match anything
  EXPECT_TRUE(matcher(opi::pair(opi::sym("a"), opi::sym("c")), bindings));

  // Check that the variable was bound correctly
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), opi::sym("c")));

  // Test non-matching pairs - 'b' doesn't match 'a' in the first position
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  EXPECT_FALSE(matcher(opi::pair(opi::sym("b"), opi::sym("c")), bindings2));
  EXPECT_TRUE(bindings2.empty());
}

// Test pattern matching with nested structures
TEST_F(MatchTest, NestedStructureMatching)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with nested pairs - 'a' is a literal, 'X' and 'Y' are variables
  opi::value pattern = opi::list(opi::sym("a"), opi::sym("X"), opi::sym("Y"));

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with a nested structure - 'a' must match exactly, 'X' and 'Y' can match anything
  EXPECT_TRUE(matcher(opi::list(opi::sym("a"), opi::sym("c"), opi::sym("d")),
                      bindings));

  // Check that the variables were bound correctly
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), opi::sym("c")));
  EXPECT_TRUE(bindings.contains(opi::sym("Y")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("Y")), opi::sym("d")));

  // Test non-matching structures - 'b' doesn't match 'a' in the first position
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  EXPECT_FALSE(matcher(opi::list(opi::sym("b"), opi::sym("c"), opi::sym("d")),
                       bindings2));
  EXPECT_TRUE(bindings2.empty());

  // Test structures of different lengths
  opi::stl::unordered_map<opi::value, opi::value> bindings3;
  EXPECT_FALSE(matcher(opi::list(opi::sym("a"), opi::sym("c")), bindings3));
  // Note: The match fails, but some bindings might have been created before the failure
  // was detected, so we don't assert that bindings3 is empty
}

// Test pattern matching with multiple variable occurrences
TEST_F(MatchTest, MultipleVariableOccurrences)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with the same variable used multiple times - 'X' is not in literals
  opi::value pattern = opi::list(opi::sym("X"), opi::sym("X"));

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with the same value for both occurrences
  // Both occurrences of 'X' must match the same value
  EXPECT_TRUE(matcher(opi::list(opi::sym("c"), opi::sym("c")), bindings));

  // Check that the variable was bound correctly
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), opi::sym("c")));

  // Test that matching fails when trying to bind the same variable to different values
  // Both occurrences of 'X' must match the same value
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  EXPECT_FALSE(matcher(opi::list(opi::sym("c"), opi::sym("d")), bindings2));
}

// Test pattern matching with different types
TEST_F(MatchTest, DifferentTypeMatching)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with a variable - 'X' is not in literals
  opi::value pattern = opi::sym("X");

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with different types - 'X' can match any value, including numbers
  EXPECT_TRUE(matcher(opi::num(42), bindings));

  // Check that the variable was bound correctly
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), opi::num(42)));

  // Test with string - 'X' can match any value, including strings
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  EXPECT_TRUE(matcher(opi::str("hello"), bindings2));
  EXPECT_TRUE(bindings2.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings2.at(opi::sym("X")), opi::str("hello")));
}

// Test the overload of operator() that doesn't take a mapping
TEST_F(MatchTest, OperatorWithoutMapping)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern - 'a' is a literal, 'X' is a variable
  opi::value pattern = opi::list(opi::sym("a"), opi::sym("X"));

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Test the overload without a mapping
  // 'a' must match exactly, 'X' can match anything
  EXPECT_TRUE(matcher(opi::list(opi::sym("a"), opi::sym("c"))));
  EXPECT_FALSE(matcher(opi::list(opi::sym("b"), opi::sym("c"))));
}

// Test pattern matching with ellipsis
TEST_F(MatchTest, EllipsisMatching)
{
  // Create a list of literals
  opi::value literals = opi::list(opi::sym("a"), opi::sym("b"));

  // Create a pattern with an ellipsis - 'X ...' matches zero or more occurrences of any value
  // The pattern is equivalent to (a X ... c) in Scheme syntax
  opi::value pattern = opi::list(opi::sym("a"), opi::sym("X"), opi::sym("..."));

  // Create a matcher
  opi::match matcher(literals, pattern);

  // Create a mapping to store variable bindings
  opi::stl::unordered_map<opi::value, opi::value> bindings;

  // Test matching with multiple elements - 'X ...' should match 'd', 'e', 'f'
  EXPECT_TRUE(matcher(
      opi::list(opi::sym("a"), opi::sym("d"), opi::sym("e"), opi::sym("f")),
      bindings));

  // Check that the variable was bound correctly to a list of matched values
  EXPECT_TRUE(bindings.contains(opi::sym("X")));
  opi::value expected_list = opi::list(opi::sym("d"), opi::sym("e"), opi::sym("f"));
  EXPECT_TRUE(opi::equal(bindings.at(opi::sym("X")), expected_list));

  // Test matching with zero elements - 'X ...' can match zero occurrences
  opi::stl::unordered_map<opi::value, opi::value> bindings2;
  EXPECT_TRUE(matcher(opi::list(opi::sym("a")), bindings2));
  
  // For empty matches, no binding is created in the map
  EXPECT_FALSE(bindings2.contains(opi::sym("X")));

  // Test matching with a single element - 'X ...' should match 'd'
  opi::stl::unordered_map<opi::value, opi::value> bindings3;
  EXPECT_TRUE(matcher(opi::list(opi::sym("a"), opi::sym("d")), bindings3));
  
  // X should be bound to a list containing just 'd'
  EXPECT_TRUE(bindings3.contains(opi::sym("X")));
  EXPECT_TRUE(opi::equal(bindings3.at(opi::sym("X")), opi::list(opi::sym("d"))));

  // Test non-matching pattern - 'b' doesn't match 'a' in the first position
  opi::stl::unordered_map<opi::value, opi::value> bindings4;
  EXPECT_FALSE(matcher(
      opi::list(opi::sym("b"), opi::sym("d"), opi::sym("e")),
      bindings4));
}

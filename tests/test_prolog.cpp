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


#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>

// Test fixture for prolog tests
class PrologTest: public ::testing::Test {
  protected:
  opi::prolog pl;

  void
  SetUp() override
  {
    // Common setup code
    pl.add_predicate(opi::list("=", "X", "X"), opi::True);
  }

  void
  TearDown() override
  {
    // Common cleanup code
  }

  // Helper to check if a query succeeds
  bool
  query_succeeds(opi::value query)
  {
    bool success = false;
    opi::predicate_runtime prt;
    pl.make_true(prt, opi::insert_cells(prt, query),
                 [&success]() { success = true; });
    return success;
  }

  // Helper to get variable bindings from a query
  std::pair<bool, opi::stl::unordered_set<opi::value>>
  query_with_bindings(opi::value query, opi::value var)
  {
    bool success = false;
    opi::stl::unordered_set<opi::value> results;

    opi::predicate_runtime prt;
    pl.make_true(prt, opi::insert_cells(prt, query), [&]() {
      success = true;
      opi::value query_result = opi::nil;
      if (opi::get_value(prt[var], query_result))
        results.emplace(query_result);
    });

    return {success, results};
  }
};

// Test adding and querying simple facts
TEST_F(PrologTest, SimpleFactQuery)
{
  // Add facts
  pl.add_predicate(opi::list("food", "pizza"), opi::True);
  pl.add_predicate(opi::list("food", "burger"), opi::True);
  pl.add_predicate(opi::list("drink", "water"), opi::True);

  // Test queries
  EXPECT_TRUE(query_succeeds(opi::list("food", "pizza")));
  EXPECT_TRUE(query_succeeds(opi::list("food", "burger")));
  EXPECT_TRUE(query_succeeds(opi::list("drink", "water")));

  // Test negative cases
  EXPECT_FALSE(query_succeeds(opi::list("food", "water")));
  EXPECT_FALSE(query_succeeds(opi::list("drink", "pizza")));
}

// Test rule evaluation
TEST_F(PrologTest, RuleEvaluation)
{
  // Add facts
  pl.add_predicate(opi::list("food", "pizza"), opi::True);
  pl.add_predicate(opi::list("food", "burger"), opi::True);
  pl.add_predicate(opi::list("drink", "water"), opi::True);

  // Add rule: meal(X) :- food(X)
  opi::value rule_head = opi::list("meal", "X");
  opi::value rule_body = opi::list("food", "X");
  pl.add_predicate(rule_head, rule_body);

  // Test rule evaluation
  EXPECT_TRUE(query_succeeds(opi::list("meal", "pizza")));
  EXPECT_TRUE(query_succeeds(opi::list("meal", "burger")));
  EXPECT_FALSE(query_succeeds(opi::list("meal", "water")));
}

// Test variable binding in queries
TEST_F(PrologTest, VariableBinding)
{
  // Add facts
  pl.add_predicate(opi::list("food", "pizza"), opi::True);
  pl.add_predicate(opi::list("food", "burger"), opi::True);
  pl.add_predicate(opi::list("drink", "water"), opi::True);

  // Query with variable
  auto [success, bindings] =
      query_with_bindings(opi::list("food", "X"), opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both pizza and burger
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);
  EXPECT_EQ(bindings.count(opi::sym("burger")), 1);
}

// Test 'and' operator
TEST_F(PrologTest, AndOperator)
{
  // Add facts
  pl.add_predicate(opi::list("food", "pizza"), opi::True);
  pl.add_predicate(opi::list("food", "burger"), opi::True);
  pl.add_predicate(opi::list("tasty", "pizza"), opi::True);

  // Create 'and' query: food(X) and tasty(X)
  opi::value and_query =
      opi::list("and", opi::list("food", "X"), opi::list("tasty", "X"));

  auto [success, bindings] = query_with_bindings(and_query, opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 1); // Only pizza should match both conditions
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);

  // Test a failing 'and' query - burger is food but not tasty
  opi::value failing_and_query = opi::list("and", opi::list("food", "burger"),
                                           opi::list("tasty", "burger"));

  EXPECT_FALSE(query_succeeds(failing_and_query));
}

// Test 'or' operator
TEST_F(PrologTest, OrOperator)
{
  // Add facts
  pl.add_predicate(opi::list("food", "pizza"), opi::True);
  pl.add_predicate(opi::list("drink", "water"), opi::True);

  // Create 'or' query: food(X) or drink(X)
  opi::value or_query =
      opi::list("or", opi::list("food", "X"), opi::list("drink", "X"));

  auto [success, bindings] = query_with_bindings(or_query, opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both pizza and water
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);
  EXPECT_EQ(bindings.count(opi::sym("water")), 1);

  // Test a failing 'or' query
  opi::value failing_or_query =
      opi::list("or", opi::list("food", "water"), opi::list("drink", "pizza"));

  EXPECT_FALSE(query_succeeds(failing_or_query));
}

// Test complex rules with multiple conditions
TEST_F(PrologTest, ComplexRules)
{
  // Add facts
  pl.add_predicate(opi::list("human-color", "white"), opi::True);
  pl.add_predicate(opi::list("human-color", "black"), opi::True);
  pl.add_predicate(opi::list("human-color", "yellow"), opi::True);

  pl.add_predicate(opi::list("origin-of", "white", "europe"), opi::True);
  pl.add_predicate(opi::list("origin-of", "black", "africa"), opi::True);
  pl.add_predicate(opi::list("origin-of", "yellow", "asia"), opi::True);
  pl.add_predicate(opi::list("origin-of", "green", "mars"), opi::True);

  // Add rule: people-inhabit(Place) :- origin-of(Color, Place), human-color(Color)
  opi::value rule_head = opi::list("people-inhabit", "Place");
  opi::value rule_body =
      opi::list("and", opi::list("origin-of", "Color", "Place"),
                opi::list("human-color", "Color"));
  pl.add_predicate(rule_head, rule_body);

  // Test rule evaluation
  EXPECT_TRUE(query_succeeds(opi::list("people-inhabit", "europe")));
  EXPECT_TRUE(query_succeeds(opi::list("people-inhabit", "africa")));
  EXPECT_TRUE(query_succeeds(opi::list("people-inhabit", "asia")));
  EXPECT_FALSE(query_succeeds(opi::list("people-inhabit", "mars")));
}

// Test match_arguments function
TEST_F(PrologTest, MatchArguments)
{
  opi::predicate_runtime prt, ert;

  // Test matching simple values
  EXPECT_TRUE(opi::match_arguments(prt, ert, opi::sym("a"), opi::sym("a")));

  EXPECT_FALSE(opi::match_arguments(prt, ert, opi::sym("a"), opi::sym("b")));

  // Test matching variables
  EXPECT_TRUE(opi::match_arguments(prt, ert,
                                   opi::insert_cells(prt, opi::sym("X")),
                                   opi::insert_cells(ert, opi::sym("Y"))));

  // Test matching lists
  EXPECT_TRUE(
      opi::match_arguments(prt, ert, opi::list("a", "b"), opi::list("a", "b")));

  EXPECT_FALSE(
      opi::match_arguments(prt, ert, opi::list("a", "b"), opi::list("a", "c")));

  // Test matching with variables in lists
  const opi::value X = opi::insert_cells(prt, opi::sym("X"));
  EXPECT_TRUE(opi::match_arguments(prt, ert, opi::list("a", X),
                                   opi::list("a", opi::sym("b"))));

  // After matching, X should be bound to b
  opi::value x_val = opi::nil;
  EXPECT_TRUE(opi::get_value(prt[opi::sym("X")], x_val));
  EXPECT_TRUE(opi::equal(x_val, opi::sym("b")));
}

// Test wildcard variable '_' behavior
TEST_F(PrologTest, WildcardVariables)
{
  // Add facts
  pl.add_predicate(opi::list("different", "a", "b"), opi::True);
  pl.add_predicate(opi::list("different", "x", "y"), opi::True);
  pl.add_predicate(opi::list("same", "a", "a"), opi::True);
  pl.add_predicate(opi::list("same", "b", "b"), opi::True);

  // Test 1: Using the same named variable twice requires the values to be the same
  opi::value same_var_query = opi::list("same", "X", "X");
  EXPECT_TRUE(query_succeeds(same_var_query));

  opi::value diff_var_query = opi::list("different", "X", "X");
  EXPECT_FALSE(query_succeeds(diff_var_query));

  // Test 2: Using wildcard '_' twice should create distinct variables
  // This should succeed because each '_' is a separate variable
  opi::value wildcard_diff_query = opi::list("different", "_", "_");
  EXPECT_TRUE(query_succeeds(wildcard_diff_query));

  // Test 3: Mixing wildcards and named variables
  // This should succeed because '_' is a distinct variable from 'X'
  opi::value mixed_query = opi::list("different", "X", "_");
  EXPECT_TRUE(query_succeeds(mixed_query));
}

// Test insert-cells predicate
TEST_F(PrologTest, InsertCellsPredicate)
{
  // Create a query using insert-cells with a quoted expression
  // We use quote to prevent automatic cell insertion during query processing
  opi::value quoted_expr = opi::cons(opi::sym("quote"), 
                                    opi::cons(opi::list("f", "X", "Y"), opi::nil));
  opi::value query = opi::list("insert-cells", quoted_expr, "Result");
  
  // Run the query and get the bindings for Result
  auto [success, bindings] = query_with_bindings(query, opi::sym("Result"));
  
  // The query should succeed
  EXPECT_TRUE(success);
  
  // There should be exactly one result
  EXPECT_EQ(bindings.size(), 1);
  
  // Get the result
  opi::value result = *bindings.begin();
  
  // The result should be a list
  EXPECT_EQ(result->t, opi::tag::pair);
  
  // The first element should be 'f'
  EXPECT_TRUE(opi::issym(opi::car(result), "f"));
  
  // The second and third elements should be cells
  opi::value second = opi::car(opi::cdr(result));
  opi::value third = opi::car(opi::cdr(opi::cdr(result)));
  
  EXPECT_EQ(second->t, opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(second), opi::CELL));
  EXPECT_EQ(second->cdr->t, opi::tag::ptr);
  
  EXPECT_EQ(third->t, opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(third), opi::CELL));
  EXPECT_EQ(third->cdr->t, opi::tag::ptr);
}

// Test insert-cells with evaluation
TEST_F(PrologTest, InsertCellsWithEvaluation)
{
  // Add a simple fact
  pl.add_predicate(opi::list("test", "value"), opi::True);
  
  // Create a quoted expression with a variable
  opi::value quoted_expr = opi::cons(opi::sym("quote"), 
                                    opi::cons(opi::list("test", "X"), opi::nil));
  
  // Create a query that:
  // 1. Uses insert-cells to insert cells into the quoted expression
  // 2. Then uses the resulting expression as a query
  opi::value query = opi::list("and", 
                              opi::list("insert-cells", quoted_expr, "Result"),
                              opi::list("call", "Result"));
  
  // Run the query and get the bindings for X
  auto [success, bindings] = query_with_bindings(query, opi::sym("X"));
  
  // The query should succeed
  EXPECT_TRUE(success);
  
  // X is a local variable so the bindings must be empty
  EXPECT_EQ(bindings.size(), 0);
}

// Test predicate with multiple branches
TEST_F(PrologTest, MultiplePredicateBranches)
{
  // Define a predicate with multiple branches (like pattern matching)
  // parent(john, bob).
  // parent(jane, bob).
  // parent(bob, alice).
  pl.add_predicate(opi::list("parent", "john", "bob"), opi::True);
  pl.add_predicate(opi::list("parent", "jane", "bob"), opi::True);
  pl.add_predicate(opi::list("parent", "bob", "alice"), opi::True);

  // Define grandparent rule
  // grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
  opi::value gp_head = opi::list("grandparent", "X", "Z");
  opi::value gp_body = opi::list("and", opi::list("parent", "X", "Y"),
                                 opi::list("parent", "Y", "Z"));
  pl.add_predicate(gp_head, gp_body);

  // Test grandparent relationship
  EXPECT_TRUE(query_succeeds(opi::list("grandparent", "john", "alice")));
  EXPECT_TRUE(query_succeeds(opi::list("grandparent", "jane", "alice")));
  EXPECT_FALSE(query_succeeds(opi::list("grandparent", "bob", "alice")));

  // Test with variable binding
  auto [success, bindings] = query_with_bindings(
      opi::list("grandparent", "X", "alice"), opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both john and jane
  EXPECT_EQ(bindings.count(opi::sym("john")), 1);
  EXPECT_EQ(bindings.count(opi::sym("jane")), 1);
}

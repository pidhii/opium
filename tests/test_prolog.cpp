#include "opium/hash.hpp" // IWYU pragma: export
#include "opium/predicate_runtime.hpp"
#include "opium/prolog.hpp"
#include "opium/stl/unordered_set.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>

// Test fixture for prolog tests
class PrologTest : public ::testing::Test {
protected:
  opi::prolog pl;

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

  // Helper to check if a query succeeds
  bool
  query_succeeds(opi::value query)
  {
    bool success = false;
    opi::predicate_runtime prt;
    pl.make_true(prt, query, [&success]() { success = true; });
    return success;
  }

  // Helper to get variable bindings from a query
  std::pair<bool, opi::unordered_set<opi::value>>
  query_with_bindings(opi::value query, opi::value var)
  {
    bool success = false;
    opi::unordered_set<opi::value> results;

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
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("pizza")), opi::True);
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("burger")), opi::True);
  pl.add_predicate(opi::list(opi::sym("drink"), opi::sym("water")), opi::True);

  // Test queries
  EXPECT_TRUE(query_succeeds(opi::list(opi::sym("food"), opi::sym("pizza"))));
  EXPECT_TRUE(query_succeeds(opi::list(opi::sym("food"), opi::sym("burger"))));
  EXPECT_TRUE(query_succeeds(opi::list(opi::sym("drink"), opi::sym("water"))));

  // Test negative cases
  EXPECT_FALSE(query_succeeds(opi::list(opi::sym("food"), opi::sym("water"))));
  EXPECT_FALSE(query_succeeds(opi::list(opi::sym("drink"), opi::sym("pizza"))));
}

// Test rule evaluation
TEST_F(PrologTest, RuleEvaluation)
{
  // Add facts
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("pizza")), opi::True);
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("burger")), opi::True);
  pl.add_predicate(opi::list(opi::sym("drink"), opi::sym("water")), opi::True);

  // Add rule: meal(X) :- food(X)
  opi::value rule_head = opi::list(opi::sym("meal"), opi::sym("X"));
  opi::value rule_body = opi::list(opi::sym("food"), opi::sym("X"));
  pl.add_predicate(rule_head, rule_body);

  // Test rule evaluation
  EXPECT_TRUE(query_succeeds(opi::list(opi::sym("meal"), opi::sym("pizza"))));
  EXPECT_TRUE(query_succeeds(opi::list(opi::sym("meal"), opi::sym("burger"))));
  EXPECT_FALSE(query_succeeds(opi::list(opi::sym("meal"), opi::sym("water"))));
}

// Test variable binding in queries
TEST_F(PrologTest, VariableBinding)
{
  // Add facts
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("pizza")), opi::True);
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("burger")), opi::True);
  pl.add_predicate(opi::list(opi::sym("drink"), opi::sym("water")), opi::True);

  // Query with variable
  auto [success, bindings] = query_with_bindings(
      opi::list(opi::sym("food"), opi::sym("X")), opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both pizza and burger
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);
  EXPECT_EQ(bindings.count(opi::sym("burger")), 1);
}

// Test 'and' operator
TEST_F(PrologTest, AndOperator)
{
  // Add facts
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("pizza")), opi::True);
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("burger")), opi::True);
  pl.add_predicate(opi::list(opi::sym("tasty"), opi::sym("pizza")), opi::True);

  // Create 'and' query: food(X) and tasty(X)
  opi::value and_query =
      opi::list(opi::sym("and"), opi::list(opi::sym("food"), opi::sym("X")),
                opi::list(opi::sym("tasty"), opi::sym("X")));

  auto [success, bindings] = query_with_bindings(and_query, opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 1); // Only pizza should match both conditions
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);

  // Test a failing 'and' query - burger is food but not tasty
  opi::value failing_and_query = opi::list(
      opi::sym("and"), opi::list(opi::sym("food"), opi::sym("burger")),
      opi::list(opi::sym("tasty"), opi::sym("burger")));

  EXPECT_FALSE(query_succeeds(failing_and_query));
}

// Test 'or' operator
TEST_F(PrologTest, OrOperator)
{
  // Add facts
  pl.add_predicate(opi::list(opi::sym("food"), opi::sym("pizza")), opi::True);
  pl.add_predicate(opi::list(opi::sym("drink"), opi::sym("water")), opi::True);

  // Create 'or' query: food(X) or drink(X)
  opi::value or_query =
      opi::list(opi::sym("or"), opi::list(opi::sym("food"), opi::sym("X")),
                opi::list(opi::sym("drink"), opi::sym("X")));

  auto [success, bindings] = query_with_bindings(or_query, opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both pizza and water
  EXPECT_EQ(bindings.count(opi::sym("pizza")), 1);
  EXPECT_EQ(bindings.count(opi::sym("water")), 1);

  // Test a failing 'or' query
  opi::value failing_or_query =
      opi::list(opi::sym("or"), opi::list(opi::sym("food"), opi::sym("water")),
                opi::list(opi::sym("drink"), opi::sym("pizza")));

  EXPECT_FALSE(query_succeeds(failing_or_query));
}

// Test complex rules with multiple conditions
TEST_F(PrologTest, ComplexRules)
{
  // Add facts
  pl.add_predicate(opi::list(opi::sym("human-color"), opi::sym("white")),
                   opi::True);
  pl.add_predicate(opi::list(opi::sym("human-color"), opi::sym("black")),
                   opi::True);
  pl.add_predicate(opi::list(opi::sym("human-color"), opi::sym("yellow")),
                   opi::True);

  pl.add_predicate(
      opi::list(opi::sym("origin-of"), opi::sym("white"), opi::sym("europe")),
      opi::True);
  pl.add_predicate(
      opi::list(opi::sym("origin-of"), opi::sym("black"), opi::sym("africa")),
      opi::True);
  pl.add_predicate(
      opi::list(opi::sym("origin-of"), opi::sym("yellow"), opi::sym("asia")),
      opi::True);
  pl.add_predicate(
      opi::list(opi::sym("origin-of"), opi::sym("green"), opi::sym("mars")),
      opi::True);

  // Add rule: people-inhabit(Place) :- origin-of(Color, Place), human-color(Color)
  opi::value rule_head =
      opi::list(opi::sym("people-inhabit"), opi::sym("Place"));
  opi::value rule_body = opi::list(
      opi::sym("and"),
      opi::list(opi::sym("origin-of"), opi::sym("Color"), opi::sym("Place")),
      opi::list(opi::sym("human-color"), opi::sym("Color")));
  pl.add_predicate(rule_head, rule_body);

  // Test rule evaluation
  EXPECT_TRUE(query_succeeds(
      opi::list(opi::sym("people-inhabit"), opi::sym("europe"))));
  EXPECT_TRUE(query_succeeds(
      opi::list(opi::sym("people-inhabit"), opi::sym("africa"))));
  EXPECT_TRUE(
      query_succeeds(opi::list(opi::sym("people-inhabit"), opi::sym("asia"))));
  EXPECT_FALSE(
      query_succeeds(opi::list(opi::sym("people-inhabit"), opi::sym("mars"))));
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
  EXPECT_TRUE(opi::match_arguments(prt, ert,
                                   opi::list(opi::sym("a"), opi::sym("b")),
                                   opi::list(opi::sym("a"), opi::sym("b"))));

  EXPECT_FALSE(opi::match_arguments(prt, ert,
                                    opi::list(opi::sym("a"), opi::sym("b")),
                                    opi::list(opi::sym("a"), opi::sym("c"))));

  // Test matching with variables in lists
  const opi::value X = opi::insert_cells(prt, opi::sym("X"));
  EXPECT_TRUE(opi::match_arguments(prt, ert, opi::list(opi::sym("a"), X),
                                   opi::list(opi::sym("a"), opi::sym("b"))));

  // After matching, X should be bound to b
  opi::value x_val = opi::nil;
  EXPECT_TRUE(opi::get_value(prt[opi::sym("X")], x_val));
  EXPECT_TRUE(opi::equal(x_val, opi::sym("b")));
}

// Test predicate with multiple branches
TEST_F(PrologTest, MultiplePredicateBranches)
{
  // Define a predicate with multiple branches (like pattern matching)
  // parent(john, bob).
  // parent(jane, bob).
  // parent(bob, alice).
  pl.add_predicate(
      opi::list(opi::sym("parent"), opi::sym("john"), opi::sym("bob")),
      opi::True);
  pl.add_predicate(
      opi::list(opi::sym("parent"), opi::sym("jane"), opi::sym("bob")),
      opi::True);
  pl.add_predicate(
      opi::list(opi::sym("parent"), opi::sym("bob"), opi::sym("alice")),
      opi::True);

  // Define grandparent rule
  // grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
  opi::value gp_head =
      opi::list(opi::sym("grandparent"), opi::sym("X"), opi::sym("Z"));
  opi::value gp_body =
      opi::list(opi::sym("and"),
                opi::list(opi::sym("parent"), opi::sym("X"), opi::sym("Y")),
                opi::list(opi::sym("parent"), opi::sym("Y"), opi::sym("Z")));
  pl.add_predicate(gp_head, gp_body);

  // Test grandparent relationship
  EXPECT_TRUE(query_succeeds(
      opi::list(opi::sym("grandparent"), opi::sym("john"), opi::sym("alice"))));
  EXPECT_TRUE(query_succeeds(
      opi::list(opi::sym("grandparent"), opi::sym("jane"), opi::sym("alice"))));
  EXPECT_FALSE(query_succeeds(
      opi::list(opi::sym("grandparent"), opi::sym("bob"), opi::sym("alice"))));

  // Test with variable binding
  auto [success, bindings] = query_with_bindings(
      opi::list(opi::sym("grandparent"), opi::sym("X"), opi::sym("alice")),
      opi::sym("X"));

  EXPECT_TRUE(success);
  EXPECT_EQ(bindings.size(), 2); // Should return both john and jane
  EXPECT_EQ(bindings.count(opi::sym("john")), 1);
  EXPECT_EQ(bindings.count(opi::sym("jane")), 1);
}

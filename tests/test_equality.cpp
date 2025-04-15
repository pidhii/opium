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


#include "opium/predicate_runtime.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>
#include <string>

namespace {

// Test fixture for equality tests
class EqualityTest: public ::testing::Test {
  protected:
  void
  SetUp() override
  {
    // Common setup code if needed
  }

  void
  TearDown() override
  {
    // Common cleanup code if needed
  }
};

// Test equality of identical objects (is)
TEST_F(EqualityTest, IdenticalObjects)
{
  opi::value val = opi::sym("test");

  // Same object should be equal
  EXPECT_TRUE(opi::is(val, val));
  EXPECT_TRUE(opi::equal(val, val));
}

// Test equality of symbols
TEST_F(EqualityTest, Symbols)
{
  opi::value sym1 = opi::sym("test");
  opi::value sym2 = opi::sym("test");
  opi::value sym3 = opi::sym("different");

  // Different objects with same symbol name should be equal
  EXPECT_FALSE(opi::is(sym1, sym2));
  EXPECT_TRUE(opi::equal(sym1, sym2));

  // Different symbol names should not be equal
  EXPECT_FALSE(opi::equal(sym1, sym3));
}

// Test equality of nil
TEST_F(EqualityTest, Nil)
{
  // nil should be equal to itself
  EXPECT_TRUE(opi::equal(opi::nil, opi::nil));

  // nil should not be equal to other types
  EXPECT_FALSE(opi::equal(opi::nil, opi::sym("nil")));
}

// Test equality of numbers
TEST_F(EqualityTest, Numbers)
{
  opi::value num1 = opi::num(42.0);
  opi::value num2 = opi::num(42.0);
  opi::value num3 = opi::num(3.14);

  // Different objects with same numeric value should be equal
  EXPECT_FALSE(opi::is(num1, num2));
  EXPECT_TRUE(opi::equal(num1, num2));

  // Different numeric values should not be equal
  EXPECT_FALSE(opi::equal(num1, num3));
}

// Test equality of pointers
TEST_F(EqualityTest, Pointers)
{
  int dummy1 = 0;
  int dummy2 = 0;

  opi::value ptr1 = opi::ptr(&dummy1);
  opi::value ptr2 = opi::ptr(&dummy1);
  opi::value ptr3 = opi::ptr(&dummy2);

  // Different objects with same pointer value should be equal
  EXPECT_FALSE(opi::is(ptr1, ptr2));
  EXPECT_TRUE(opi::equal(ptr1, ptr2));

  // Different pointer values should not be equal
  EXPECT_FALSE(opi::equal(ptr1, ptr3));
}

// Test equality of strings
TEST_F(EqualityTest, Strings)
{
  opi::value str1 = opi::str("hello");
  opi::value str2 = opi::str("hello");
  opi::value str3 = opi::str("world");

  // Different objects with same string content should be equal
  EXPECT_FALSE(opi::is(str1, str2));
  EXPECT_TRUE(opi::equal(str1, str2));

  // Different string contents should not be equal
  EXPECT_FALSE(opi::equal(str1, str3));
}

// Test equality of pairs
TEST_F(EqualityTest, Pairs)
{
  opi::value pair1 = opi::pair(opi::num(1), opi::num(2));
  opi::value pair2 = opi::pair(opi::num(1), opi::num(2));
  opi::value pair3 = opi::pair(opi::num(1), opi::num(3));

  // Different objects with same pair content should be equal
  EXPECT_FALSE(opi::is(pair1, pair2));
  EXPECT_TRUE(opi::equal(pair1, pair2));

  // Different pair contents should not be equal
  EXPECT_FALSE(opi::equal(pair1, pair3));
}

// Test equality of nested pairs
TEST_F(EqualityTest, NestedPairs)
{
  opi::value inner1 = opi::pair(opi::num(1), opi::num(2));
  opi::value inner2 = opi::pair(opi::num(1), opi::num(2));
  opi::value outer1 = opi::pair(inner1, opi::sym("end"));
  opi::value outer2 = opi::pair(inner2, opi::sym("end"));

  // Nested pairs with same structure should be equal
  EXPECT_TRUE(opi::equal(outer1, outer2));
}

// Test equality of lists
TEST_F(EqualityTest, Lists)
{
  opi::value list1 = opi::list(opi::num(1), opi::num(2), opi::num(3));
  opi::value list2 = opi::list(opi::num(1), opi::num(2), opi::num(3));
  opi::value list3 = opi::list(opi::num(1), opi::num(2), opi::num(4));

  // Different lists with same content should be equal
  EXPECT_FALSE(opi::is(list1, list2));
  EXPECT_TRUE(opi::equal(list1, list2));

  // Different list contents should not be equal
  EXPECT_FALSE(opi::equal(list1, list3));
}

// Test equality of booleans
TEST_F(EqualityTest, Booleans)
{
  // Boolean constants should be equal to themselves
  EXPECT_TRUE(opi::equal(opi::True, opi::True));
  EXPECT_TRUE(opi::equal(opi::False, opi::False));

  // True and False should not be equal
  EXPECT_FALSE(opi::equal(opi::True, opi::False));
}

// Test equality of different types
TEST_F(EqualityTest, DifferentTypes)
{
  opi::value sym = opi::sym("test");
  opi::value num = opi::num(42.0);
  opi::value str = opi::str("test");

  // Different types should not be equal
  EXPECT_FALSE(opi::equal(sym, num));
  EXPECT_FALSE(opi::equal(sym, str));
  EXPECT_FALSE(opi::equal(num, str));
}

// Test equality with cyclic structures
TEST_F(EqualityTest, CyclicStructures)
{
  // Create two cyclic lists: (a . <cycle>)
  opi::value cycle1 = opi::pair(opi::sym("a"), opi::nil);
  opi::value cycle2 = opi::pair(opi::sym("a"), opi::nil);

  // Make them cyclic by setting cdr to point to themselves
  cycle1->cdr = &*cycle1;
  cycle2->cdr = &*cycle2;

  // Cyclic structures with same pattern should be equal
  EXPECT_TRUE(opi::equal(cycle1, cycle2));

  // Create a different cyclic structure: (b . <cycle>)
  opi::value cycle3 = opi::pair(opi::sym("b"), opi::nil);
  cycle3->cdr = &*cycle3;

  // Different cyclic structures should not be equal
  EXPECT_FALSE(opi::equal(cycle1, cycle3));
}

// Test equality with shared substructures
TEST_F(EqualityTest, SharedSubstructures)
{
  opi::value shared = opi::pair(opi::num(1), opi::num(2));

  // Create two structures that share a substructure
  opi::value struct1 = opi::pair(shared, opi::sym("end"));
  opi::value struct2 = opi::pair(shared, opi::sym("end"));

  // Structures with shared substructures should be equal
  EXPECT_TRUE(opi::equal(struct1, struct2));

  // The shared substructure should be identical (same object)
  EXPECT_TRUE(opi::is(opi::car(struct1), opi::car(struct2)));
}

} // anonymous namespace

// Test fixture for equivalence tests
class EquivalenceTest: public ::testing::Test {
  protected:
  opi::predicate_runtime prt;

  void
  SetUp() override
  {
    // Common setup code if needed
  }

  void
  TearDown() override
  {
    // Common cleanup code if needed
  }
};

// Test equivalence of identical objects
TEST_F(EquivalenceTest, IdenticalObjects)
{
  opi::value val = opi::sym("test");

  // Same object should be equivalent
  EXPECT_TRUE(opi::equivalent(val, val));
}

// Test equivalence of simple values
TEST_F(EquivalenceTest, SimpleValues)
{
  opi::value sym1 = opi::sym("test");
  opi::value sym2 = opi::sym("test");
  opi::value num1 = opi::num(42.0);
  opi::value num2 = opi::num(42.0);

  // Equal values should be equivalent
  EXPECT_TRUE(opi::equivalent(sym1, sym2));
  EXPECT_TRUE(opi::equivalent(num1, num2));

  // Different values should not be equivalent
  EXPECT_FALSE(opi::equivalent(sym1, num1));
}

// Test equivalence with unbound variables
TEST_F(EquivalenceTest, UnboundVariables)
{
  // Create two unbound variables
  opi::value var1 = opi::sym("X");
  opi::value var2 = opi::sym("Y");

  // Insert cells for the variables
  opi::value cell1 = opi::insert_cells(prt, var1);
  opi::value cell2 = opi::insert_cells(prt, var2);

  // Unbound variables are not equivalent to concrete values
  EXPECT_FALSE(opi::equivalent(cell1, opi::sym("value")));

  // Two different unbound variables are considered equivalent
  // (they can be unified with each other)
  EXPECT_TRUE(opi::equivalent(cell1, cell2));
}

// Test equivalence with bound variables
TEST_F(EquivalenceTest, BoundVariables)
{
  // Create variables
  opi::value var1 = opi::sym("X");
  opi::value var2 = opi::sym("Y");

  // Insert cells for the variables
  opi::value cell1 = opi::insert_cells(prt, var1);
  opi::value cell2 = opi::insert_cells(prt, var2);

  // Bind var1 to a value
  opi::unify(prt[var1], prt.make_term(opi::sym("value")));

  // cell1 should now be equivalent to the value
  opi::value val = opi::nil;
  EXPECT_TRUE(opi::get_value(prt[var1], val));
  EXPECT_TRUE(opi::equivalent(cell1, val));

  // A bound variable is not equivalent to an unbound variable
  EXPECT_FALSE(opi::equivalent(cell1, cell2));

  // Bind var2 to the same value
  opi::unify(prt[var2], prt.make_term(opi::sym("value")));

  // Now cell1 and cell2 should be equivalent
  EXPECT_TRUE(opi::equivalent(cell1, cell2));
}

// Test equivalence with complex structures
TEST_F(EquivalenceTest, ComplexStructures)
{
  // Create variables
  opi::value var1 = opi::sym("X");
  opi::value var2 = opi::sym("Y");

  // Create complex structures with variables
  opi::value struct1 = opi::list(opi::sym("f"), var1, opi::num(1));
  opi::value struct2 = opi::list(opi::sym("f"), var2, opi::num(1));

  // Insert cells for the structures
  opi::value cell_struct1 = opi::insert_cells(prt, struct1);
  opi::value cell_struct2 = opi::insert_cells(prt, struct2);

  // Structures with unbound variables should be equivalent
  EXPECT_TRUE(opi::equivalent(cell_struct1, cell_struct2));

  // Binding one variable to a value should make them non-equivalent
  opi::unify(prt[var1], prt.make_term(opi::sym("value")));
  EXPECT_FALSE(opi::equivalent(cell_struct1, cell_struct2));

  // Binding the other variable to the same value should make them equivalent again
  opi::unify(prt[var2], prt.make_term(opi::sym("value")));
  EXPECT_TRUE(opi::equivalent(cell_struct1, cell_struct2));
}

// Test equivalence with cyclic structures
TEST_F(EquivalenceTest, CyclicStructures)
{
  // Create variables
  opi::value var1 = opi::sym("X");
  opi::value var2 = opi::sym("Y");

  // Create structures that will become cyclic
  opi::value struct1 = opi::list(opi::sym("f"), var1);
  opi::value struct2 = opi::list(opi::sym("f"), var2);

  // Insert cells for the structures
  opi::value cell_struct1 = opi::insert_cells(prt, struct1);
  opi::value cell_struct2 = opi::insert_cells(prt, struct2);

  // Make them cyclic by binding variables to the structures themselves
  opi::unify(prt[var1], prt.make_term(cell_struct1));
  opi::unify(prt[var2], prt.make_term(cell_struct2));

  // The cyclic structures should be equivalent
  EXPECT_TRUE(opi::equivalent(cell_struct1, cell_struct2));
}

// Test equivalence with different variable bindings
TEST_F(EquivalenceTest, DifferentVariableBindings)
{
  // Create variables
  opi::value var1 = opi::sym("X");
  opi::value var2 = opi::sym("Y");
  opi::value var3 = opi::sym("Z");

  // Create structures with variables
  opi::value struct1 = opi::list(opi::sym("f"), var1, var1); // Same variable used twice
  opi::value struct2 = opi::list(opi::sym("f"), var2, var3); // Different variables

  // Insert cells for the structures
  opi::value cell_struct1 = opi::insert_cells(prt, struct1);
  opi::value cell_struct2 = opi::insert_cells(prt, struct2);

  // These structures should not be equivalent because of different variable patterns
  EXPECT_FALSE(opi::equivalent(cell_struct1, cell_struct2));
}

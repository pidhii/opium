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

#include "opium/lisp_parser.hpp"
#include "opium/value.hpp"
#include "../osl/operators.hpp"

#include <gtest/gtest.h>

namespace {

// Helper function to create a simple operators library for testing
opi::osl::operators_library
create_test_operators() {
  opi::osl::operators_library oplib;

  // Add common infix operators with various precedences
  // Negative precedence = left-to-right associativity
  // Positive precedence = right-to-left associativity
  // The higher the absolute value, the lower is the precedence
  oplib.add_operator(opi::osl::operator_kind::infix, "=", -50);  // Assignment (left-to-right, lowest precedence)
  oplib.add_operator(opi::osl::operator_kind::infix, "+", -20);  // Addition (left-to-right)
  oplib.add_operator(opi::osl::operator_kind::infix, "-", -20);  // Subtraction (left-to-right)
  oplib.add_operator(opi::osl::operator_kind::infix, "*", -10);  // Multiplication (left-to-right, higher precedence)
  oplib.add_operator(opi::osl::operator_kind::infix, "/", -10);  // Division (left-to-right, higher precedence)
  oplib.add_operator(opi::osl::operator_kind::infix, "^", 5);    // Exponentiation (right-to-left, even higher precedence)

  // Add prefix operators (sufix in the code)
  oplib.add_operator(opi::osl::operator_kind::sufix, "-", 7);    // Unary minus 
  oplib.add_operator(opi::osl::operator_kind::sufix, "!", 7);    // Logical NOT
  oplib.add_operator(opi::osl::operator_kind::sufix, "++", 7);   // Pre-increment

  // Add postfix operators
  oplib.add_operator(opi::osl::operator_kind::postfix, "++", 7); // Post-increment
  oplib.add_operator(opi::osl::operator_kind::postfix, "!", 7);  // Factorial
  oplib.add_operator(opi::osl::operator_kind::postfix, "[]", 3); // Array access (higher precedence)

  return oplib;
}

// Helper function to create an unresolved infix expression
opi::value
make_infix(opi::value op, opi::value lhs, opi::value rhs)
{
  return opi::list("unresolved-infix", op, lhs, rhs);
}

// Helper function to create an unresolved sufix (prefix) expression
opi::value
make_sufix(opi::value op, opi::value arg)
{
  return opi::list("unresolved-sufix", op, arg);
}

// Helper function to create an unresolved postfix expression
opi::value
make_postfix(opi::value op, opi::value arg)
{
  return opi::list("unresolved-postfix", op, arg);
}

TEST(OperatorPrecedenceTest, OperatorsLibrary)
{
  const opi::osl::operators_library oplib = create_test_operators();
  opi::osl::operator_definition opdef;
  ASSERT_TRUE(oplib.find_operator(opi::osl::operator_kind::infix, "+", opdef));
  ASSERT_TRUE(oplib.find_operator(opi::osl::operator_kind::infix, "*", opdef));
  ASSERT_EQ(opdef.precedence, -10);
}

// Test basic infix operator precedence (multiplication before addition)
TEST(OperatorPrecedenceTest, BasicInfixPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 1 + 2 * 3
  // Should be resolved as: (+ 1 (* 2 3)), not (* (+ 1 2) 3)
  opi::value expr =
      make_infix("+", opi::num(1), make_infix("*", opi::num(2), opi::num(3)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ 1 (* 2 3))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test left-to-right associativity for same precedence operators
TEST(OperatorPrecedenceTest, LeftToRightAssociativity)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 10 - 5 - 2
  // Should be resolved as: (- (- 10 5) 2) = 3, not (- 10 (- 5 2)) = 7
  opi::value expr = make_infix(
      "-", make_infix("-", opi::num(10.0), opi::num(5.0)), opi::num(2.0));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(- (- 10 5) 2)"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test right-to-left associativity for exponentiation
TEST(OperatorPrecedenceTest, RightToLeftAssociativity)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 2 ^ 3 ^ 2
  // Should be resolved as: (^ 2 (^ 3 2)) = 2 ^ 9 = 512, not (^ (^ 2 3) 2) = 8 ^ 2 = 64
  opi::value expr = make_infix("^", opi::num(2.0),
                               make_infix("^", opi::num(3.0), opi::num(2.0)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(^ 2 (^ 3 2))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test complex precedence with multiple operators
TEST(OperatorPrecedenceTest, ComplexPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 1 + 2 * 3 - 4 / 2
  // Should be: (- (+ 1 (* 2 3)) (/ 4 2))
  opi::value expr =
      make_infix("-",
                 make_infix("+", opi::num(1.0),
                            make_infix("*", opi::num(2.0), opi::num(3.0))),
                 make_infix("/", opi::num(4.0), opi::num(2.0)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(- (+ 1 (* 2 3)) (/ 4 2))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test prefix operator with higher precedence than infix
TEST(OperatorPrecedenceTest, PrefixHigherPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: -5 * 3
  // Should be: (* (- 5) 3), not (- (* 5 3))
  opi::value expr =
      make_infix("*", make_sufix("-", opi::num(5.0)), opi::num(3.0));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(* (- 5) 3)"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test prefix operator with lower precedence than infix
TEST(OperatorPrecedenceTest, PrefixLowerPrecedence)
{
  opi::osl::operators_library oplib = create_test_operators();

  // Create a prefix operator with very low precedence
  oplib.add_operator(opi::osl::operator_kind::sufix, "~",
                     5); // Very low precedence

  // Expression: ~(5 + 3)
  // Should be: (+ (~ 5) 3)
  opi::value expr = make_sufix("~", make_infix("+", opi::num(5), opi::num(3)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ (~ 5) 3)"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test postfix operator with higher precedence
TEST(OperatorPrecedenceTest, PostfixHigherPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: x[] + 5
  // Should be: (+ ([] x) 5), not ([] (+ x 5))
  opi::value expr = make_infix("+", make_postfix("[]", "x"), opi::num(5.0));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ ([] x) 5)"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test mixing prefix and postfix operators - should throw due to ambiguity
TEST(OperatorPrecedenceTest, MixedPrefixPostfix)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: -x!
  // Should throw due to same precedence ambiguity
  opi::value expr = make_sufix("-", make_postfix("!", "x"));

  ASSERT_THROW(opi::osl::resolve_operator_precedence(expr, oplib),
               opi::bad_code);
}

// Test operator precedence with parentheses (expressions)
TEST(OperatorPrecedenceTest, WithParentheses)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 1 + 2 * 3
  // Should be: 1 + (2 * 3)
  opi::value sum = make_infix("+", opi::num(1), opi::num(2));
  opi::value expr = make_infix("*", sum, opi::num(3));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ 1 (* 2 3))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test deeply nested operators
TEST(OperatorPrecedenceTest, DeeplyNested)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 1 + 2 * 3 + 4 * 5
  // Should be: (+ (+ 1 (* 2 3)) (* 4 5))
  opi::value expr =
      make_infix("+",
                 make_infix("+", opi::num(1.0),
                            make_infix("*", opi::num(2.0), opi::num(3.0))),
                 make_infix("*", opi::num(4.0), opi::num(5.0)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ (+ 1 (* 2 3)) (* 4 5))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test operator precedence with assignment
TEST(OperatorPrecedenceTest, AssignmentPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: x = 5 + 3
  // Should be: (= x (+ 5 3)), not (+ (= x 5) 3)
  opi::value expr =
      make_infix("=", "x", make_infix("+", opi::num(5.0), opi::num(3.0)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(= x (+ 5 3))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test chained assignments (left-to-right associativity)
TEST(OperatorPrecedenceTest, ChainedAssignment)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: a = b = 5
  // Should be: (= (= a b) 5) due to left-to-right associativity
  opi::value expr = make_infix("=", make_infix("=", "a", "b"), opi::num(5.0));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(= (= a b) 5)"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test that ambiguous operators throw errors
TEST(OperatorPrecedenceTest, AmbiguousSufixInfix)
{
  // Create an ambiguous scenario: sufix and infix with same precedence
  opi::osl::operators_library ambiguous_oplib;
  ambiguous_oplib.add_operator(opi::osl::operator_kind::sufix, "-", 10);
  ambiguous_oplib.add_operator(opi::osl::operator_kind::infix, "+", 10);

  // Expression: -(x + y) where - and + have same precedence
  opi::value expr = make_sufix("-", make_infix("+", "x", "y"));

  // This should throw an error due to ambiguity
  ASSERT_THROW(opi::osl::resolve_operator_precedence(expr, ambiguous_oplib),
               opi::bad_code);
}

// Test that ambiguous postfix-infix operators throw errors
TEST(OperatorPrecedenceTest, AmbiguousPostfixInfix)
{
  // Create an ambiguous scenario: postfix and infix with same precedence
  opi::osl::operators_library ambiguous_oplib;
  ambiguous_oplib.add_operator(opi::osl::operator_kind::postfix, "!", 10);
  ambiguous_oplib.add_operator(opi::osl::operator_kind::infix, "+", 10);

  // Expression: x + y! where + and ! have same precedence
  opi::value expr = make_infix("+", "x", make_postfix("!", "y"));

  // This should throw an error due to ambiguity
  ASSERT_THROW(opi::osl::resolve_operator_precedence(expr, ambiguous_oplib),
               opi::bad_code);
}

// Test multiple consecutive prefix operators
TEST(OperatorPrecedenceTest, ConsecutivePrefixOperators)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: --x (double negation)
  opi::value expr = make_sufix("-", make_sufix("-", "x"));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(- (- x))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test multiple consecutive postfix operators
TEST(OperatorPrecedenceTest, ConsecutivePostfixOperators)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: x++! (increment then factorial)
  opi::value expr = make_postfix("!", make_postfix("++", "x"));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(! (++ x))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test expressions with no operators (plain values)
TEST(OperatorPrecedenceTest, PlainValue)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Just a number
  opi::value expr = opi::num(42.0);

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);

  ASSERT_TRUE(opi::isnum(result));
  ASSERT_EQ(opi::num_val(result), 42.0);
}

// Test expressions with symbols
TEST(OperatorPrecedenceTest, SymbolValue)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Just a symbol
  opi::value expr = "variable";

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);

  ASSERT_TRUE(opi::issym(result));
  ASSERT_TRUE(opi::issym(result, "variable"));
}

// Test complex expression with all operator types
TEST(OperatorPrecedenceTest, AllOperatorTypes)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: -a + b! * c
  // Should be: (+ (- a) (* (! b) c))
  opi::value expr = make_infix("+", make_sufix("-", "a"),
                               make_infix("*", make_postfix("!", "b"), "c"));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ (- a) (* (! b) c))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test operator precedence with exponentiation
TEST(OperatorPrecedenceTest, ExponentiationPrecedence)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 2 * 3 ^ 2
  // Should be: (* 2 (^ 3 2)) = 2 * 9 = 18, not (^ (* 2 3) 2) = 6 ^ 2 = 36
  opi::value expr = make_infix("*", opi::num(2.0),
                               make_infix("^", opi::num(3.0), opi::num(2.0)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(* 2 (^ 3 2))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test mixed left and right associative operators
TEST(OperatorPrecedenceTest, MixedAssociativity)
{
  const opi::osl::operators_library oplib = create_test_operators();

  // Expression: 2 + 3 * 4 ^ 2 ^ 3
  // Should be: (+ 2 (* 3 (^ 4 (^ 2 3))))
  opi::value expr = make_infix(
      "+", opi::num(2.0),
      make_infix("*", opi::num(3.0),
                 make_infix("^", opi::num(4.0),
                            make_infix("^", opi::num(2.0), opi::num(3.0)))));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  opi::value expected = "(+ 2 (* 3 (^ 4 (^ 2 3))))"_lisp;

  ASSERT_TRUE(result == expected);
}

// Test precedence reordering - input needs rebalancing
TEST(OperatorPrecedenceTest, NeedsRebalancing)
{
  const opi::osl::operators_library oplib = create_test_operators();
  opi::value expr =
      make_infix("*", opi::num(1), make_infix("+", opi::num(2), opi::num(3)));

  opi::value result = opi::osl::resolve_operator_precedence(expr, oplib);
  // Should be rebalanced to: (+ (* 1 2) 3)
  opi::value expected = "(+ (* 1 2) 3)"_lisp;

  ASSERT_TRUE(result == expected);
}

} // anonymous namespace

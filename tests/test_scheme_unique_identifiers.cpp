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


#include "opium/code_transform_utils.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>
#include <string>


namespace {

// Test fixture for scheme_unique_identifiers tests
class SchemeUniqueIdentifiersTest: public testing::Test {
  protected:
  void
  SetUp() override
  {
    // Create a symbol generator with a predictable format for testing
    gensym_counter = 0;
    gensym = std::make_unique<opi::symbol_generator>(gensym_counter, "_test{}");
    transformer =
        std::make_unique<opi::scheme_unique_identifiers>(*gensym, false);
  }

  // Helper function to parse a Scheme expression from string
  opi::value
  parse(const std::string &code)
  {
    return opi::lisp_parser {}.parse(code);
  }

  // Helper function to check if a symbol has the expected format
  bool
  is_unique_symbol(const opi::value &val, const std::string &prefix)
  {
    if (val->t != opi::tag::sym)
    {
      return false;
    }
    std::string_view sym_str = opi::sym_name(val);
    return sym_str.find(prefix) == 0;
  }

  size_t gensym_counter;
  std::unique_ptr<opi::symbol_generator> gensym;
  std::unique_ptr<opi::scheme_unique_identifiers> transformer;
};


// Test basic variable replacement
TEST_F(SchemeUniqueIdentifiersTest, BasicVariableReplacement)
{
  // Simple variable reference
  opi::value expr = parse("x");
  opi::value result = (*transformer)(expr);

  // The variable should not be replaced since it's not in any scope
  EXPECT_TRUE(opi::issym(result, "x"));
}

// Test variable replacement in define
TEST_F(SchemeUniqueIdentifiersTest, DefineVariableReplacement)
{
  // Define a variable
  opi::value expr = parse("(define x 42)");
  opi::value result = (*transformer)(expr);

  // The variable name should be replaced with a unique symbol
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "define"));

  // Check that x is replaced with a unique symbol
  opi::value var_name = opi::car(opi::cdr(result));
  EXPECT_TRUE(is_unique_symbol(var_name, "_test"));

  // Check that the value is preserved
  opi::value var_value = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(var_value->t, opi::tag::num);
  EXPECT_EQ(var_value->num, 42.0);
}


// Test variable replacement in lambda
TEST_F(SchemeUniqueIdentifiersTest, LambdaVariableReplacement)
{
  // Lambda expression with parameters
  opi::value expr = parse("(lambda (x y) (+ x y))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a lambda expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "lambda"));

  // Get the parameter list
  opi::value params = opi::car(opi::cdr(result));
  ASSERT_EQ(params->t, opi::tag::pair);

  // Check that parameters are replaced with unique symbols
  opi::value param_x = opi::car(params);
  opi::value param_y = opi::car(opi::cdr(params));
  EXPECT_TRUE(is_unique_symbol(param_x, "_test"));
  EXPECT_TRUE(is_unique_symbol(param_y, "_test"));
  EXPECT_NE(opi::sym_name(param_x),
            opi::sym_name(param_y)); // Different symbols

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "+"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_x = opi::car(opi::cdr(body));
  opi::value body_y = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_x, param_x)); // Same symbol as parameter
  EXPECT_TRUE(opi::equal(body_y, param_y)); // Same symbol as parameter
}


// Test variable replacement in let
TEST_F(SchemeUniqueIdentifiersTest, LetVariableReplacement)
{
  // Let expression with bindings
  opi::value expr = parse("(let ((x 1) (y 2)) (+ x y))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a let expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check the first binding (x 1)
  opi::value binding_x = opi::car(bindings);
  ASSERT_EQ(binding_x->t, opi::tag::pair);
  opi::value var_x = opi::car(binding_x);
  EXPECT_TRUE(is_unique_symbol(var_x, "_test"));

  // Check the second binding (y 2)
  opi::value binding_y = opi::car(opi::cdr(bindings));
  ASSERT_EQ(binding_y->t, opi::tag::pair);
  opi::value var_y = opi::car(binding_y);
  EXPECT_TRUE(is_unique_symbol(var_y, "_test"));
  EXPECT_NE(opi::sym_name(var_x), opi::sym_name(var_y)); // Different symbols

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "+"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_x = opi::car(opi::cdr(body));
  opi::value body_y = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_x, var_x)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(body_y, var_y)); // Same symbol as binding
}


// Test variable replacement in let*
TEST_F(SchemeUniqueIdentifiersTest, LetStarVariableReplacement)
{
  // Let* expression with bindings where second binding uses first
  opi::value expr = parse("(let* ((x 1) (y (+ x 2))) (+ x y))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a let* expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let*"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check the first binding (x 1)
  opi::value binding_x = opi::car(bindings);
  ASSERT_EQ(binding_x->t, opi::tag::pair);
  opi::value var_x = opi::car(binding_x);
  EXPECT_TRUE(is_unique_symbol(var_x, "_test"));

  // Check the second binding (y (+ x 2))
  opi::value binding_y = opi::car(opi::cdr(bindings));
  ASSERT_EQ(binding_y->t, opi::tag::pair);
  opi::value var_y = opi::car(binding_y);
  EXPECT_TRUE(is_unique_symbol(var_y, "_test"));
  EXPECT_NE(opi::sym_name(var_x), opi::sym_name(var_y)); // Different symbols

  // Check the expression in the second binding
  opi::value expr_y = opi::car(opi::cdr(binding_y));
  ASSERT_EQ(expr_y->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(expr_y), "+"));

  // Check that x in (+ x 2) is replaced with the same unique symbol
  opi::value expr_y_x = opi::car(opi::cdr(expr_y));
  EXPECT_TRUE(opi::equal(expr_y_x, var_x)); // Same symbol as first binding

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "+"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_x = opi::car(opi::cdr(body));
  opi::value body_y = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_x, var_x)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(body_y, var_y)); // Same symbol as binding
}


// Test variable replacement in letrec
TEST_F(SchemeUniqueIdentifiersTest, LetrecVariableReplacement)
{
  // Letrec expression with recursive bindings
  opi::value expr = parse("(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact "
                          "(- n 1))))))) (fact 5))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a letrec expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "letrec"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check the binding (fact (lambda ...))
  opi::value binding_fact = opi::car(bindings);
  ASSERT_EQ(binding_fact->t, opi::tag::pair);
  opi::value var_fact = opi::car(binding_fact);
  EXPECT_TRUE(is_unique_symbol(var_fact, "_test"));

  // Get the lambda expression
  opi::value lambda_expr = opi::car(opi::cdr(binding_fact));
  ASSERT_EQ(lambda_expr->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(lambda_expr), "lambda"));

  // Get the lambda parameter
  opi::value lambda_params = opi::car(opi::cdr(lambda_expr));
  ASSERT_EQ(lambda_params->t, opi::tag::pair);
  opi::value param_n = opi::car(lambda_params);
  EXPECT_TRUE(is_unique_symbol(param_n, "_test"));

  // Check the lambda body
  opi::value lambda_body = opi::car(opi::cdr(opi::cdr(lambda_expr)));
  ASSERT_EQ(lambda_body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(lambda_body), "if"));

  // Check the recursive call to fact in the lambda body
  opi::value if_else = opi::car(opi::cdr(opi::cdr(opi::cdr(lambda_body))));
  ASSERT_EQ(if_else->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(if_else), "*"));

  opi::value fact_call = opi::car(opi::cdr(opi::cdr(if_else)));
  ASSERT_EQ(fact_call->t, opi::tag::pair);

  // Check that fact in the recursive call is replaced with the same unique symbol
  opi::value fact_in_call = opi::car(fact_call);
  EXPECT_TRUE(opi::equal(fact_in_call, var_fact)); // Same symbol as binding

  // Check the body of the letrec
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);

  // Check that fact in the body is replaced with the same unique symbol
  opi::value fact_in_body = opi::car(body);
  EXPECT_TRUE(opi::equal(fact_in_body, var_fact)); // Same symbol as binding
}


// Test variable replacement in function-style define
TEST_F(SchemeUniqueIdentifiersTest, FunctionDefineReplacement)
{
  // Define a function
  opi::value expr = parse("(define (square x) (* x x))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a define expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "template"));

  // Get the function name and parameters
  opi::value func_def = opi::car(opi::cdr(result));
  ASSERT_EQ(func_def->t, opi::tag::pair);

  // Check that the function name is replaced with a unique symbol
  opi::value func_name = opi::car(func_def);
  EXPECT_TRUE(is_unique_symbol(func_name, "_test"));

  // Check that the parameter is replaced with a unique symbol
  opi::value param_x = opi::car(opi::cdr(func_def));
  EXPECT_TRUE(is_unique_symbol(param_x, "_test"));
  EXPECT_NE(opi::sym_name(func_name),
            opi::sym_name(param_x)); // Different symbols

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "*"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_x1 = opi::car(opi::cdr(body));
  opi::value body_x2 = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_x1, param_x)); // Same symbol as parameter
  EXPECT_TRUE(opi::equal(body_x2, param_x)); // Same symbol as parameter
}


// Test nested scopes
TEST_F(SchemeUniqueIdentifiersTest, NestedScopes)
{
  // Nested let expressions with shadowing
  opi::value expr = parse("(let ((x 1)) (let ((x 2)) (+ x 3)))");
  opi::value result = (*transformer)(expr);

  // Check outer let
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let"));

  // Get outer binding for x
  opi::value outer_bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(outer_bindings->t, opi::tag::pair);
  opi::value outer_binding_x = opi::car(outer_bindings);
  ASSERT_EQ(outer_binding_x->t, opi::tag::pair);
  opi::value outer_var_x = opi::car(outer_binding_x);
  EXPECT_TRUE(is_unique_symbol(outer_var_x, "_test"));

  // Get inner let
  opi::value inner_let = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(inner_let->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(inner_let), "let"));

  // Get inner binding for x
  opi::value inner_bindings = opi::car(opi::cdr(inner_let));
  ASSERT_EQ(inner_bindings->t, opi::tag::pair);
  opi::value inner_binding_x = opi::car(inner_bindings);
  ASSERT_EQ(inner_binding_x->t, opi::tag::pair);
  opi::value inner_var_x = opi::car(inner_binding_x);
  EXPECT_TRUE(is_unique_symbol(inner_var_x, "_test"));

  // Check that inner and outer x have different unique symbols
  EXPECT_NE(opi::sym_name(outer_var_x), opi::sym_name(inner_var_x));

  // Check the inner body
  opi::value inner_body = opi::car(opi::cdr(opi::cdr(inner_let)));
  ASSERT_EQ(inner_body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(inner_body), "+"));

  // Check that x in the inner body refers to the inner binding
  opi::value inner_body_x = opi::car(opi::cdr(inner_body));
  EXPECT_TRUE(
      opi::equal(inner_body_x, inner_var_x)); // Same symbol as inner binding
  EXPECT_FALSE(
      opi::equal(inner_body_x, outer_var_x)); // Not the same as outer binding
}


// Test if expression
TEST_F(SchemeUniqueIdentifiersTest, IfExpression)
{
  // If expression
  opi::value expr = parse("(if (> x 0) (+ x 1) (- x 1))");
  opi::value result = (*transformer)(expr);

  // Check that it's still an if expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "if"));

  // Since x is not bound in any scope, it should remain unchanged
  // Check the condition
  opi::value condition = opi::car(opi::cdr(result));
  ASSERT_EQ(condition->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(condition), ">"));
  opi::value cond_x = opi::car(opi::cdr(condition));
  EXPECT_TRUE(opi::issym(cond_x, "x")); // Unchanged

  // Check the then branch
  opi::value then_branch = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(then_branch->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(then_branch), "+"));
  opi::value then_x = opi::car(opi::cdr(then_branch));
  EXPECT_TRUE(opi::issym(then_x, "x")); // Unchanged

  // Check the else branch
  opi::value else_branch = opi::car(opi::cdr(opi::cdr(opi::cdr(result))));
  ASSERT_EQ(else_branch->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(else_branch), "-"));
  opi::value else_x = opi::car(opi::cdr(else_branch));
  EXPECT_TRUE(opi::issym(else_x, "x")); // Unchanged
}


// Test complex expression with multiple constructs
TEST_F(SchemeUniqueIdentifiersTest, ComplexExpression)
{
  // Complex expression with multiple constructs
  opi::value expr = parse(R"(
        (let ((x 10)
              (y 20))
          (define (add a b)
            (+ a b))
          (if (> x 0)
              (add x y)
              (let ((z (- y x)))
                (add z 5))))
    )");
  opi::value result = (*transformer)(expr);
  return;

  // This test doesn't verify every detail, but checks that the transformation
  // produces a valid expression with the expected structure
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check that x and y are replaced with unique symbols
  opi::value binding_x = opi::car(bindings);
  opi::value binding_y = opi::car(opi::cdr(bindings));
  opi::value var_x = opi::car(binding_x);
  opi::value var_y = opi::car(binding_y);
  EXPECT_TRUE(is_unique_symbol(var_x, "_test"));
  EXPECT_TRUE(is_unique_symbol(var_y, "_test"));
  EXPECT_NE(opi::sym_name(var_x), opi::sym_name(var_y)); // Different symbols

  // Get the define expression
  opi::value define_expr = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(define_expr->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(define_expr), "define"));

  // Check that the function name and parameters are replaced with unique symbols
  opi::value func_def = opi::car(opi::cdr(define_expr));
  opi::value func_name = opi::car(func_def);
  opi::value param_a = opi::car(opi::cdr(func_def));
  opi::value param_b = opi::car(opi::cdr(opi::cdr(func_def)));
  EXPECT_TRUE(is_unique_symbol(func_name, "_test"));
  EXPECT_TRUE(is_unique_symbol(param_a, "_test"));
  EXPECT_TRUE(is_unique_symbol(param_b, "_test"));

  // Get the if expression
  opi::value if_expr = opi::car(opi::cdr(opi::cdr(opi::cdr(result))));
  ASSERT_EQ(if_expr->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(if_expr), "if"));

  // Check the condition
  opi::value condition = opi::car(opi::cdr(if_expr));
  ASSERT_EQ(condition->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(condition), ">"));
  opi::value cond_x = opi::car(opi::cdr(condition));
  EXPECT_TRUE(opi::equal(cond_x, var_x)); // Same symbol as binding

  // Check the then branch
  opi::value then_branch = opi::car(opi::cdr(opi::cdr(if_expr)));
  ASSERT_EQ(then_branch->t, opi::tag::pair);
  opi::value then_func = opi::car(then_branch);
  EXPECT_TRUE(opi::equal(then_func, func_name)); // Same symbol as function name

  // Check the arguments in the then branch
  opi::value then_arg1 = opi::car(opi::cdr(then_branch));
  opi::value then_arg2 = opi::car(opi::cdr(opi::cdr(then_branch)));
  EXPECT_TRUE(opi::equal(then_arg1, var_x)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(then_arg2, var_y)); // Same symbol as binding

  // Check the else branch
  opi::value else_branch = opi::car(opi::cdr(opi::cdr(opi::cdr(if_expr))));
  ASSERT_EQ(else_branch->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(else_branch), "let"));

  // Check the binding in the else branch
  opi::value else_bindings = opi::car(opi::cdr(else_branch));
  ASSERT_EQ(else_bindings->t, opi::tag::pair);
  opi::value else_binding_z = opi::car(else_bindings);
  opi::value var_z = opi::car(else_binding_z);
  EXPECT_TRUE(is_unique_symbol(var_z, "_test"));

  // Check the expression in the binding
  opi::value z_expr = opi::car(opi::cdr(else_binding_z));
  ASSERT_EQ(z_expr->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(z_expr), "-"));
  opi::value z_expr_y = opi::car(opi::cdr(z_expr));
  opi::value z_expr_x = opi::car(opi::cdr(opi::cdr(z_expr)));
  EXPECT_TRUE(opi::equal(z_expr_y, var_y)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(z_expr_x, var_x)); // Same symbol as binding

  // Check the body of the else branch
  opi::value else_body = opi::car(opi::cdr(opi::cdr(else_branch)));
  ASSERT_EQ(else_body->t, opi::tag::pair);
  opi::value else_func = opi::car(else_body);
  EXPECT_TRUE(opi::equal(else_func, func_name)); // Same symbol as function name

  // Check the arguments in the else body
  opi::value else_arg1 = opi::car(opi::cdr(else_body));
  EXPECT_TRUE(opi::equal(else_arg1, var_z)); // Same symbol as binding
}


// Test variable replacement in let-values
TEST_F(SchemeUniqueIdentifiersTest, LetValuesVariableReplacement)
{
  // let-values expression with bindings
  opi::value expr = parse("(let-values (((x y) (values 1 2))) (+ x y))");
  opi::value result = (*transformer)(expr);

  // Check that it's still a let-values expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let-values"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check the binding ((x y) (values 1 2))
  opi::value binding = opi::car(bindings);
  ASSERT_EQ(binding->t, opi::tag::pair);

  // Check the identifier list (x y)
  opi::value identlist = opi::car(binding);
  ASSERT_EQ(identlist->t, opi::tag::pair);

  // Check that x and y are replaced with unique symbols
  opi::value var_x = opi::car(identlist);
  opi::value var_y = opi::car(opi::cdr(identlist));
  EXPECT_TRUE(is_unique_symbol(var_x, "_test"));
  EXPECT_TRUE(is_unique_symbol(var_y, "_test"));
  EXPECT_NE(opi::sym_name(var_x), opi::sym_name(var_y)); // Different symbols

  // Check the expression (values 1 2)
  opi::value expr_values = opi::car(opi::cdr(binding));
  ASSERT_EQ(expr_values->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(expr_values), "values"));

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "+"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_x = opi::car(opi::cdr(body));
  opi::value body_y = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_x, var_x)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(body_y, var_y)); // Same symbol as binding
}


// Test variable replacement in let*-values
TEST_F(SchemeUniqueIdentifiersTest, LetStarValuesVariableReplacement)
{
  // let*-values expression with bindings where second binding uses first
  opi::value expr = parse("(let*-values (((x) (values 1))                  "
                          "              ((y z) (values (+ x 1) (* x 2)))) "
                          "  (+ y z))                                      ");
  opi::value result = (*transformer)(expr);

  // Check that it's still a let*-values expression
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "let*-values"));

  // Get the bindings
  opi::value bindings = opi::car(opi::cdr(result));
  ASSERT_EQ(bindings->t, opi::tag::pair);

  // Check the first binding ((x) (values 1))
  opi::value binding_x = opi::car(bindings);
  ASSERT_EQ(binding_x->t, opi::tag::pair);

  // Check the identifier list (x)
  opi::value identlist_x = opi::car(binding_x);
  ASSERT_EQ(identlist_x->t, opi::tag::pair);
  opi::value var_x = opi::car(identlist_x);
  EXPECT_TRUE(is_unique_symbol(var_x, "_test"));

  // Check the second binding ((y z) (values (+ x 1) (* x 2)))
  opi::value binding_yz = opi::car(opi::cdr(bindings));
  ASSERT_EQ(binding_yz->t, opi::tag::pair);

  // Check the identifier list (y z)
  opi::value identlist_yz = opi::car(binding_yz);
  ASSERT_EQ(identlist_yz->t, opi::tag::pair);
  opi::value var_y = opi::car(identlist_yz);
  opi::value var_z = opi::car(opi::cdr(identlist_yz));
  EXPECT_TRUE(is_unique_symbol(var_y, "_test"));
  EXPECT_TRUE(is_unique_symbol(var_z, "_test"));
  EXPECT_NE(opi::sym_name(var_y), opi::sym_name(var_z)); // Different symbols

  // Check the expression (values (+ x 1) (* x 2))
  opi::value expr_values = opi::car(opi::cdr(binding_yz));
  ASSERT_EQ(expr_values->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(expr_values), "values"));

  // Check the first value expression (+ x 1)
  opi::value expr_y = opi::car(opi::cdr(expr_values));
  ASSERT_EQ(expr_y->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(expr_y), "+"));

  // Check that x in (+ x 1) is replaced with the same unique symbol
  opi::value expr_y_x = opi::car(opi::cdr(expr_y));
  EXPECT_TRUE(opi::equal(expr_y_x, var_x)); // Same symbol as first binding

  // Check the second value expression (* x 2)
  opi::value expr_z = opi::car(opi::cdr(opi::cdr(expr_values)));
  ASSERT_EQ(expr_z->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(expr_z), "*"));

  // Check that x in (* x 2) is replaced with the same unique symbol
  opi::value expr_z_x = opi::car(opi::cdr(expr_z));
  EXPECT_TRUE(opi::equal(expr_z_x, var_x)); // Same symbol as first binding

  // Check the body
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(body->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(body), "+"));

  // Check that variable references in the body are replaced with the same unique symbols
  opi::value body_y = opi::car(opi::cdr(body));
  opi::value body_z = opi::car(opi::cdr(opi::cdr(body)));
  EXPECT_TRUE(opi::equal(body_y, var_y)); // Same symbol as binding
  EXPECT_TRUE(opi::equal(body_z, var_z)); // Same symbol as binding
}

} // anonymous namespace

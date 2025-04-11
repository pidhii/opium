#include "opium/lisp_parser.hpp"
#include "opium/predicate_runtime.hpp"
#include "opium/logging.hpp"

#include <gtest/gtest.h>
#include <sstream>


// Test fixture for quotation handling in insert_cells
class QuotationTest: public ::testing::Test {
  protected:
  opi::lisp_parser parser;
  opi::predicate_runtime prt;

  // Helper function to parse a string into a value
  opi::value
  parse(const std::string &input)
  {
    std::istringstream iss(input);
    return parser.parse(iss);
  }

  // Helper function to check if a value contains a cell
  bool
  contains_cell(opi::value expr)
  {
    if (expr->t == opi::tag::pair)
    {
      if (opi::issym(opi::car(expr), opi::CELL))
        return true;
      return contains_cell(opi::car(expr)) || contains_cell(opi::cdr(expr));
    }
    return false;
  }
};

// Test that regular expressions have cells inserted for variables
TEST_F(QuotationTest, RegularExpressionCellInsertion)
{
  // Parse a regular expression with a variable
  opi::value expr = parse("(define X 42)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // Verify that cells were inserted (for the variable X)
  EXPECT_TRUE(contains_cell(result));
}

// Test that quoted expressions don't have cells inserted
TEST_F(QuotationTest, QuotedExpressionNoCellInsertion)
{
  // Parse a quoted expression
  opi::value expr = parse("'(X Y Z)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // Verify that the quoted expression is preserved as-is (no cells inserted)
  EXPECT_FALSE(contains_cell(result));

  // Verify that the structure is preserved (quoted content is returned directly)
  EXPECT_TRUE(result->t == opi::tag::pair);
  EXPECT_TRUE(opi::car(result)->t == opi::tag::sym);
  EXPECT_TRUE(opi::issym(opi::car(result), "X"));
}

// Test quasiquotation with unquote
TEST_F(QuotationTest, QuasiquoteWithUnquote)
{
  // Parse a quasiquoted expression with an unquoted variable
  opi::value expr = parse("`(1 2 ,X 4)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // Verify that cells were inserted only in the unquoted part
  EXPECT_TRUE(contains_cell(result));

  // The result should be a list with the unquoted variable replaced with a cell
  EXPECT_TRUE(result->t == opi::tag::pair);
  
  // Navigate to the third element (the unquoted X)
  opi::value third_element = opi::car(opi::cdr(opi::cdr(result)));
  
  // Verify it's a cell
  EXPECT_TRUE(opi::issym(opi::car(third_element), opi::CELL));
}

// Test that unquote-splicing throws an error
TEST_F(QuotationTest, UnquoteSplicingThrowsError)
{
  // Parse a quasiquoted expression with unquote-splicing
  opi::value expr = parse("`(1 2 ,@Xs 4)");

  // Insert cells should throw an error for unquote-splicing
  EXPECT_THROW(opi::insert_cells(prt, expr), opi::bad_code);
}

// Test nested quotation
TEST_F(QuotationTest, NestedQuotation)
{
  // Parse a nested quoted expression
  opi::value expr = parse("'(a b '(X Y) c)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // Verify that no cells were inserted in the nested quoted expression
  EXPECT_FALSE(contains_cell(result));
  
  // The result should be a list with the quoted content directly
  EXPECT_TRUE(result->t == opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(result), "a"));
}

// Test nested quasiquotation
TEST_F(QuotationTest, NestedQuasiquotation)
{
  // Parse a nested quasiquoted expression with unquotes
  opi::value expr = parse("`(a b `(c ,X ,(+ Y 1)) d)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // Verify that cells were not inserted in the inner quasiquote
  // (since it's at quasiquote level 1, inner unquotes are at level 2)
  EXPECT_FALSE(contains_cell(result));
  
  // The result should be a list
  EXPECT_TRUE(result->t == opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(result), "a"));
  
  // The third element should be a quasiquote
  opi::value third_element = opi::car(opi::cdr(opi::cdr(result)));
  EXPECT_TRUE(third_element->t == opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(third_element), "quasiquote"));
}
// Test unquote outside quasiquote throws error
TEST_F(QuotationTest, UnquoteOutsideQuasiquoteThrowsError)
{
  // Parse an expression with unquote outside quasiquote
  opi::value expr = parse("(a b ,X c)");

  // Insert cells should throw an error
  EXPECT_THROW(opi::insert_cells(prt, expr), opi::bad_code);
}

// Test double-nested quasiquote with unquote
TEST_F(QuotationTest, DoubleNestedQuasiquote)
{
  // Parse a double-nested quasiquoted expression
  opi::value expr = parse("``(a b ,c ,,d)");

  // Insert cells
  opi::value result = opi::insert_cells(prt, expr);

  // The result should be a quasiquote
  EXPECT_TRUE(result->t == opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(result), "quasiquote"));

  // The inner unquote (,c) should be preserved
  opi::value inner_list = opi::car(opi::cdr(result));
  opi::value third_element = opi::car(opi::cdr(opi::cdr(inner_list)));
  EXPECT_TRUE(third_element->t == opi::tag::pair);
  EXPECT_TRUE(opi::issym(opi::car(third_element), "unquote"));
  
  // The double-unquote (,,d) should have one unquote removed
  opi::value fourth_element = opi::car(opi::cdr(opi::cdr(opi::cdr(inner_list))));
  EXPECT_TRUE(fourth_element == opi::list("unquote", "d"));
}

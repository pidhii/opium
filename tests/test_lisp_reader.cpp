#include "opium/lisp_reader.hpp"
#include "opium/lisp_parser.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>
#include <string>


// Test basic functionality of lisp_reader
TEST(LispReaderTest, BasicFunctionality) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add a simple expression
  reader << "(1 2 3)";
  
  // Extract the value
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  
  // Verify the result
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling multiple expressions
TEST(LispReaderTest, MultipleExpressions) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add multiple expressions
  reader << "(1 2) (3 4)";
  
  // Extract first expression
  opi::value result1 = opi::nil;
  ASSERT_TRUE(reader >> result1);
  ASSERT_EQ(result1->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result1)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result1)->num, 1.0);
  
  // Extract second expression
  opi::value result2 = opi::nil;
  ASSERT_TRUE(reader >> result2);
  ASSERT_EQ(result2->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result2)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result2)->num, 3.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result2);
}

// Test handling partial inputs
TEST(LispReaderTest, PartialInputs) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add a partial expression
  reader << "(1 2";
  
  // No complete expression should be available yet
  opi::value result = opi::nil;
  ASSERT_FALSE(reader >> result);
  
  // Add the rest of the expression
  reader << " 3)";
  
  // Now we should be able to extract the complete expression
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling complex partial inputs
TEST(LispReaderTest, ComplexPartialInputs) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add a partial nested expression
  reader << "(define (fac n) (if (= n 0)";
  
  // No complete expression should be available yet
  opi::value result = opi::nil;
  ASSERT_FALSE(reader >> result);
  
  // Add more of the expression
  reader << " 1 (* n";
  
  // Still no complete expression
  ASSERT_FALSE(reader >> result);
  
  // Complete the expression
  reader << " (fac (- n 1)))))";
  
  // Now we should be able to extract the complete expression
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "define"));
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling multiple partial inputs
TEST(LispReaderTest, MultiplePartialInputs) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add partial expressions
  reader << "(1 2) (3";
  
  // Extract the first complete expression
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // No more complete expressions should be available
  ASSERT_FALSE(reader >> result);
  
  // Complete the second expression
  reader << " 4)";
  
  // Now we should be able to extract the second expression
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 3.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling empty input
TEST(LispReaderTest, EmptyInput) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add empty input
  reader << "";
  
  // No values should be available
  opi::value result = opi::nil;
  ASSERT_FALSE(reader >> result);
}

// Test handling whitespace-only input
TEST(LispReaderTest, WhitespaceInput) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add whitespace-only input
  reader << "   \n\t   ";
  
  // No values should be available
  opi::value result = opi::nil;
  ASSERT_FALSE(reader >> result);
}

// Test handling comments
TEST(LispReaderTest, CommentsInput) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add input with comments
  reader << "; This is a comment\n(1 2) ; Another comment\n";
  
  // Extract the expression
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling quoted expressions
TEST(LispReaderTest, QuotedExpressions) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add a quoted expression
  reader << "'(1 2 3)";
  
  // Extract the value
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  
  // Should be (quote (1 2 3))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quote"));
  
  // Check the quoted list
  opi::value quoted = opi::car(opi::cdr(result));
  ASSERT_EQ(quoted->t, opi::tag::pair);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling different types of values
TEST(LispReaderTest, DifferentValueTypes) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add expressions with different types
  reader << "symbol 42 \"string\" #t ()";
  
  // Extract and verify symbol
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::sym);
  ASSERT_TRUE(opi::issym(result, "symbol"));
  
  // Extract and verify number
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::num);
  ASSERT_EQ(result->num, 42.0);
  
  // Extract and verify string
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::str);
  ASSERT_TRUE(opi::isstr(result, "string"));
  
  // Extract and verify boolean
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::boolean);
  ASSERT_EQ(result->boolean, true);
  
  // Extract and verify nil
  ASSERT_TRUE(reader >> result);
  ASSERT_EQ(result->t, opi::tag::nil);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling improper lists
TEST(LispReaderTest, ImproperLists) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add an improper list
  reader << "(1 2 . 3)";
  
  // Extract the value
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  
  // Verify the result
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // Check second element
  opi::value second = opi::car(opi::cdr(result));
  ASSERT_EQ(second->t, opi::tag::num);
  ASSERT_EQ(second->num, 2.0);
  
  // Check improper ending
  opi::value third = opi::cdr(opi::cdr(result));
  ASSERT_EQ(third->t, opi::tag::num);
  ASSERT_EQ(third->num, 3.0);
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

// Test handling nested expressions
TEST(LispReaderTest, NestedExpressions) {
  opi::lisp_parser parser;
  opi::lisp_reader reader(parser);
  
  // Add a nested expression
  reader << "(let ((x 1) (y 2)) (+ x y))";
  
  // Extract the value
  opi::value result = opi::nil;
  ASSERT_TRUE(reader >> result);
  
  // Verify the result is a pair
  ASSERT_EQ(result->t, opi::tag::pair);
  
  // Check first element is 'let'
  ASSERT_TRUE(opi::issym(opi::car(result), "let"));
  
  // No more values should be available
  ASSERT_FALSE(reader >> result);
}

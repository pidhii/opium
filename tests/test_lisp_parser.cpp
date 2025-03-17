#include "opium/lisp_parser.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>
#include <sstream>
#include <string>

namespace {

// Test parsing from string
TEST(LispParserTest, ParseFromString) {
  const std::string text = "(1 2 3 . 4)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Verify the result is a pair
  ASSERT_EQ(result->t, opi::tag::pair);
  
  // Check first element
  ASSERT_EQ(opi::car(result)->t, opi::tag::num);
  ASSERT_EQ(opi::car(result)->num, 1.0);
  
  // Check second element
  opi::value second = opi::car(opi::cdr(result));
  ASSERT_EQ(second->t, opi::tag::num);
  ASSERT_EQ(second->num, 2.0);
  
  // Check third element
  opi::value third = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(third->t, opi::tag::num);
  ASSERT_EQ(third->num, 3.0);
  
  // Check fourth element (improper list ending)
  opi::value fourth = opi::cdr(opi::cdr(opi::cdr(result)));
  ASSERT_EQ(fourth->t, opi::tag::num);
  ASSERT_EQ(fourth->num, 4.0);
}

// Test parsing from stream
TEST(LispParserTest, ParseFromStream) {
  std::istringstream iss("(define (factorial n)\n"
                         "  (if (= n 0)\n"
                         "      1\n"
                         "      (* n (factorial (- n 1)))))");
  
  opi::value result = opi::lisp_parser{}.parse(iss);
  
  // Verify the result is a pair
  ASSERT_EQ(result->t, opi::tag::pair);
  
  // Check that the first element is 'define'
  ASSERT_EQ(opi::car(result)->t, opi::tag::sym);
  ASSERT_TRUE(opi::issym(opi::car(result), "define"));
}

// Test parsing a symbol
TEST(LispParserTest, ParseSymbol) {
  const std::string text = "symbol";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  ASSERT_EQ(result->t, opi::tag::sym);
  ASSERT_TRUE(opi::issym(result, "symbol"));
}

// Test parsing a number
TEST(LispParserTest, ParseNumber) {
  const std::string text = "42.5";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  ASSERT_EQ(result->t, opi::tag::num);
  ASSERT_EQ(result->num, 42.5);
}

// Test parsing a string
TEST(LispParserTest, ParseString) {
  const std::string text = "\"hello, world\"";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  ASSERT_EQ(result->t, opi::tag::str);
  ASSERT_TRUE(opi::isstr(result, "hello, world"));
}

// Test parsing a quoted expression
TEST(LispParserTest, ParseQuote) {
  const std::string text = "'(1 2 3)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Should be (quote (1 2 3))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quote"));
  
  // Check the quoted list
  opi::value quoted = opi::car(opi::cdr(result));
  ASSERT_EQ(quoted->t, opi::tag::pair);
  
  // Check elements of the quoted list
  ASSERT_EQ(opi::car(quoted)->t, opi::tag::num);
  ASSERT_EQ(opi::car(quoted)->num, 1.0);
  
  ASSERT_EQ(opi::car(opi::cdr(quoted))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(quoted))->num, 2.0);
  
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->num, 3.0);
}

// Test parsing nested lists
TEST(LispParserTest, ParseNestedLists) {
  const std::string text = "(a (b c) d)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Check first element
  ASSERT_TRUE(opi::issym(opi::car(result), "a"));
  
  // Check second element (nested list)
  opi::value nested = opi::car(opi::cdr(result));
  ASSERT_EQ(nested->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(nested), "b"));
  ASSERT_TRUE(opi::issym(opi::car(opi::cdr(nested)), "c"));
  
  // Check third element
  ASSERT_TRUE(opi::issym(opi::car(opi::cdr(opi::cdr(result))), "d"));
}

// Test parsing with comments
TEST(LispParserTest, ParseWithComments) {
  const std::string text = "(a ; comment\n b)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "a"));
  ASSERT_TRUE(opi::issym(opi::car(opi::cdr(result)), "b"));
}

// Test parsing error handling
TEST(LispParserTest, ParseErrorHandling) {
  // Unbalanced parentheses
  const std::string text = "(a (b c)";
  ASSERT_THROW(opi::lisp_parser{}.parse(text), opi::parse_error);
}

} // anonymous namespace

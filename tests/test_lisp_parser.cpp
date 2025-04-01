#include "opium/lisp_parser.hpp"
#include "opium/value.hpp"

#include <gtest/gtest.h>
#include <fstream>
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

// Test parsing quasiquote
TEST(LispParserTest, ParseQuasiquote) {
  const std::string text = "`(1 2 3)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Should be (quasiquote (1 2 3))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quasiquote"));
  
  // Check the quasiquoted list
  opi::value quoted = opi::car(opi::cdr(result));
  ASSERT_EQ(quoted->t, opi::tag::pair);
  
  // Check elements of the quasiquoted list
  ASSERT_EQ(opi::car(quoted)->t, opi::tag::num);
  ASSERT_EQ(opi::car(quoted)->num, 1.0);
  
  ASSERT_EQ(opi::car(opi::cdr(quoted))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(quoted))->num, 2.0);
  
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->num, 3.0);
}

// Test parsing unquote
TEST(LispParserTest, ParseUnquote) {
  const std::string text = "`(1 ,x 3)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Should be (quasiquote (1 (unquote x) 3))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quasiquote"));
  
  // Check the quasiquoted list
  opi::value quoted = opi::car(opi::cdr(result));
  ASSERT_EQ(quoted->t, opi::tag::pair);
  
  // First element should be 1
  ASSERT_EQ(opi::car(quoted)->t, opi::tag::num);
  ASSERT_EQ(opi::car(quoted)->num, 1.0);
  
  // Second element should be (unquote x)
  opi::value second = opi::car(opi::cdr(quoted));
  ASSERT_EQ(second->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(second), "unquote"));
  ASSERT_TRUE(opi::issym(opi::car(opi::cdr(second)), "x"));
  
  // Third element should be 3
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->num, 3.0);
}

// Test parsing unquote-splicing
TEST(LispParserTest, ParseUnquoteSplicing) {
  const std::string text = "`(1 ,@xs 3)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Should be (quasiquote (1 (unquote-splicing xs) 3))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quasiquote"));
  
  // Check the quasiquoted list
  opi::value quoted = opi::car(opi::cdr(result));
  ASSERT_EQ(quoted->t, opi::tag::pair);
  
  // First element should be 1
  ASSERT_EQ(opi::car(quoted)->t, opi::tag::num);
  ASSERT_EQ(opi::car(quoted)->num, 1.0);
  
  // Second element should be (unquote-splicing xs)
  opi::value second = opi::car(opi::cdr(quoted));
  ASSERT_EQ(second->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(second), "unquote-splicing"));
  ASSERT_TRUE(opi::issym(opi::car(opi::cdr(second)), "xs"));
  
  // Third element should be 3
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(quoted)))->num, 3.0);
}

// Test parsing nested quasiquote
TEST(LispParserTest, ParseNestedQuasiquote) {
  const std::string text = "`(1 `(2 ,(+ 1 2)) 4)";
  opi::value result = opi::lisp_parser{}.parse(text);
  
  // Should be (quasiquote (1 (quasiquote (2 (unquote (+ 1 2)))) 4))
  ASSERT_EQ(result->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(result), "quasiquote"));
  
  // Check the outer quasiquoted list
  opi::value outer = opi::car(opi::cdr(result));
  ASSERT_EQ(outer->t, opi::tag::pair);
  
  // First element should be 1
  ASSERT_EQ(opi::car(outer)->t, opi::tag::num);
  ASSERT_EQ(opi::car(outer)->num, 1.0);
  
  // Second element should be (quasiquote (2 (unquote (+ 1 2))))
  opi::value second = opi::car(opi::cdr(outer));
  ASSERT_EQ(second->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(second), "quasiquote"));
  
  // Check the inner quasiquoted list
  opi::value inner = opi::car(opi::cdr(second));
  ASSERT_EQ(inner->t, opi::tag::pair);
  
  // First element of inner list should be 2
  ASSERT_EQ(opi::car(inner)->t, opi::tag::num);
  ASSERT_EQ(opi::car(inner)->num, 2.0);
  
  // Second element of inner list should be (unquote (+ 1 2))
  opi::value innerSecond = opi::car(opi::cdr(inner));
  ASSERT_EQ(innerSecond->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(innerSecond), "unquote"));
  
  // Check the unquoted expression (+ 1 2)
  opi::value unquoted = opi::car(opi::cdr(innerSecond));
  ASSERT_EQ(unquoted->t, opi::tag::pair);
  ASSERT_TRUE(opi::issym(opi::car(unquoted), "+"));
  
  // Third element of outer list should be 4
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(outer)))->t, opi::tag::num);
  ASSERT_EQ(opi::car(opi::cdr(opi::cdr(outer)))->num, 4.0);
}

// Test location tracking
TEST(LispParserTest, LocationTracking) {
  opi::lisp_parser parser;
  const std::string text = "(define (square x) (* x x))";
  opi::value result = parser.parse(text);
  
  // Check location for the entire expression
  opi::source_location loc;
  ASSERT_TRUE(parser.get_location(result, loc));
  ASSERT_EQ(loc.start, 0);
  ASSERT_EQ(loc.end, text.length());
  
  // Check location for 'define'
  opi::value define_sym = opi::car(result);
  ASSERT_TRUE(parser.get_location(define_sym, loc));
  ASSERT_EQ(loc.start, 1);
  ASSERT_EQ(loc.end, 7); // "define" is 6 chars
  
  // Check location for the function signature (square x)
  opi::value signature = opi::car(opi::cdr(result));
  ASSERT_TRUE(parser.get_location(signature, loc));
  ASSERT_EQ(loc.start, 8);
  ASSERT_EQ(loc.end, 18); // "(square x)" is 10 chars
  
  // Check location for 'square'
  opi::value square_sym = opi::car(signature);
  ASSERT_TRUE(parser.get_location(square_sym, loc));
  ASSERT_EQ(loc.start, 9);
  ASSERT_EQ(loc.end, 15); // "square" is 6 chars
  
  // Check location for 'x' in the signature
  opi::value x_param = opi::car(opi::cdr(signature));
  ASSERT_TRUE(parser.get_location(x_param, loc));
  ASSERT_EQ(loc.start, 16);
  ASSERT_EQ(loc.end, 17); // "x" is 1 char
  
  // Check location for the function body (* x x)
  opi::value body = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_TRUE(parser.get_location(body, loc));
  ASSERT_EQ(loc.start, 19);
  ASSERT_EQ(loc.end, 26); // "(* x x)" is 7 chars
  
  // Check location for '*'
  opi::value multiply = opi::car(body);
  ASSERT_TRUE(parser.get_location(multiply, loc));
  ASSERT_EQ(loc.start, 20);
  ASSERT_EQ(loc.end, 21); // "*" is 1 char
  
  // Check location for first 'x' in the body
  opi::value x1 = opi::car(opi::cdr(body));
  ASSERT_TRUE(parser.get_location(x1, loc));
  ASSERT_EQ(loc.start, 22);
  ASSERT_EQ(loc.end, 23); // "x" is 1 char
  
  // Check location for second 'x' in the body
  opi::value x2 = opi::car(opi::cdr(opi::cdr(body)));
  ASSERT_TRUE(parser.get_location(x2, loc));
  ASSERT_EQ(loc.start, 24);
  ASSERT_EQ(loc.end, 25); // "x" is 1 char
}

// Test location tracking with nested expressions
TEST(LispParserTest, NestedLocationTracking) {
  opi::lisp_parser parser;
  const std::string text = "(a (b c) d)";
  opi::value result = parser.parse(text);
  
  // Check location for the entire expression
  opi::source_location loc;
  ASSERT_TRUE(parser.get_location(result, loc));
  ASSERT_EQ(loc.start, 0);
  ASSERT_EQ(loc.end, text.length());
  
  // Check location for 'a'
  opi::value a_sym = opi::car(result);
  ASSERT_TRUE(parser.get_location(a_sym, loc));
  ASSERT_EQ(loc.start, 1);
  ASSERT_EQ(loc.end, 2); // "a" is 1 char
  
  // Check location for '(b c)'
  opi::value bc_list = opi::car(opi::cdr(result));
  ASSERT_TRUE(parser.get_location(bc_list, loc));
  ASSERT_EQ(loc.start, 3);
  ASSERT_EQ(loc.end, 8); // "(b c)" is 5 chars
  
  // Check location for 'b'
  opi::value b_sym = opi::car(bc_list);
  ASSERT_TRUE(parser.get_location(b_sym, loc));
  ASSERT_EQ(loc.start, 4);
  ASSERT_EQ(loc.end, 5); // "b" is 1 char
  
  // Check location for 'c'
  opi::value c_sym = opi::car(opi::cdr(bc_list));
  ASSERT_TRUE(parser.get_location(c_sym, loc));
  ASSERT_EQ(loc.start, 6);
  ASSERT_EQ(loc.end, 7); // "c" is 1 char
  
  // Check location for 'd'
  opi::value d_sym = opi::car(opi::cdr(opi::cdr(result)));
  ASSERT_TRUE(parser.get_location(d_sym, loc));
  ASSERT_EQ(loc.start, 9);
  ASSERT_EQ(loc.end, 10); // "d" is 1 char
}

// Test display_location function
TEST(LispParserTest, DisplayLocation) {
  opi::lisp_parser parser;
  const std::string text = "(define (square x) (* x x))";
  opi::value result = parser.parse(text);
  
  // Get location for the entire expression
  opi::source_location loc;
  ASSERT_TRUE(parser.get_location(result, loc));
  
  // Test display_location with a string source
  std::string display = display_location(loc);
  ASSERT_FALSE(display.empty());
  ASSERT_TRUE(display.find("in <string>") != std::string::npos);
  
  // Create a temporary file for testing file-based location display
  std::string temp_filename = "test_location_display.scm";
  {
    std::ofstream temp_file(temp_filename);
    temp_file << text << std::endl;
  }
  
  // Parse from the file
  std::ifstream file(temp_filename);
  result = parser.parse(file, temp_filename);
  
  // Get location for the entire expression
  ASSERT_TRUE(parser.get_location(result, loc));
  
  // Test display_location with a file source
  display = display_location(loc);
  ASSERT_FALSE(display.empty());
  ASSERT_TRUE(display.find("in " + temp_filename) != std::string::npos);
  
  // Clean up
  std::remove(temp_filename.c_str());
}

} // anonymous namespace

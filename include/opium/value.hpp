#pragma once

#include "opium/memory.hpp"

#include <string>
#include <ostream>
#include <cstring>
#include <ranges>

/**
 * \file value.hpp
 * Core value representation for the Opium library
 * 
 * This file defines the fundamental value types and operations used throughout
 * the Opium library.
 * 
 * \ingroup core
 */



namespace opi {


/**
 * Tag enumeration for object types
 * 
 * \ingroup core
 */
enum class tag {
  nil,
  sym,
  str,
  num,
  ptr,
  pair,
  boolean,
};

struct object;

/**
 * Value class representing a reference to an object
 * 
 * \ingroup core
 */
class value {
  public:
  /**
   * Constructor
   * 
   * \param ptr Pointer to the object
   */
  explicit
  value(object *ptr): m_ptr {ptr} { }

  value(const char *sym);

  /**
   * Access the object via pointer syntax
   * 
   * \return Pointer to the object
   */
  constexpr object*
  operator -> () const noexcept
  { return m_ptr; }

  /**
   * Dereference the value
   * 
   * \return Reference to the object
   */
  constexpr object&
  operator * () const noexcept
  { return *m_ptr; }

  /**
   * Equality comparison
   * 
   * \param other Value to compare with
   * \return True if the values are equal
   */
  bool
  operator == (opi::value other) const noexcept;

  private:
  object *m_ptr; /**< Pointer to the object */
}; // class opi::value


/**
 * Object structure representing a value
 * 
 * \ingroup core
 */
struct object {
  /**
   * Constructor
   * 
   * \note This constructor is unsafe
   * \param tag Type tag for the object
   */
  object(tag tag): t {tag} { }

  tag t; /**< Type tag */
  union {
    struct { char *data; size_t len; } sym; /**< Symbol data */
    struct { char *data; size_t len; } str; /**< String data */
    struct { object *car, *cdr; }; /**< Pair data */
    long double num; /**< Numeric data */
    void *ptr; /**< Pointer data */
    bool boolean; /**< Boolean data */
  };
}; // struct opi::object


/**
 * \name Fundamental constructors
 * \{
 */

/**
 * Create a symbol value
 * 
 * \param str Symbol name
 * \return Symbol value
 *
 * \ingroup core
 */
inline value
sym(std::string_view str)
{
  value ret {make<object>(tag::sym)};
  ret->sym.data = static_cast<char*>(allocate_atomic(str.length() + 1));
  std::memcpy(ret->sym.data, str.data(), str.length() + 1);
  ret->sym.len = str.length();
  return ret;
}

/**
 * Create a string value
 * 
 * \param str String content
 * \return String value
 *
 * \ingroup core
 */
inline value
str(const std::string &str)
{
  value ret {make<object>(tag::str)};
  ret->str.data = static_cast<char*>(allocate_atomic(str.length() + 1));
  std::memcpy(ret->str.data, str.c_str(), str.length() + 1);
  ret->str.len = str.length();
  return ret;
}

/**
 * Create a numeric value
 * 
 * \param val Numeric value
 * \return Numeric value
 *
 * \ingroup core
 */
inline value
num(long double val)
{
  value ret {make_atomic<object>(tag::num)};
  ret->num = val;
  return ret;
}

/**
 * Create a pointer value
 * 
 * \param ptr Pointer value
 * \return Pointer value
 *
 * \ingroup core
 */
inline value
ptr(void *ptr)
{
  value ret {make<object>(tag::ptr)};
  ret->ptr = ptr;
  return ret;
}

/**
 * Create a pair value
 * 
 * \param car First element of the pair
 * \param cdr Second element of the pair
 * \return Pair value
 *
 * \ingroup core
 */
inline value
pair(value car, value cdr)
{
  value ret {make<object>(tag::pair)};
  ret->car = &*car;
  ret->cdr = &*cdr;
  return ret;
}

extern const value True, False; /**< Boolean constants */
extern const value nil; /**< Nil constant */

/** \} */

/**
 * \name Overloaded constructors from C++ types (casts)
 * \{
 */

/**
 * Convert a number to a value
 * 
 * \param x Number to convert
 * \return Numeric value
 *
 * \ingroup core
 */
inline value
from(long double x)
{ return num(x); }

/**
 * Convert a pair to a value
 * 
 * \param p Pair to convert
 * \return Pair value
 *
 * \ingroup core
 */
inline value
from(const std::pair<value, value> &p)
{ return pair(p.first, p.second); }

/**
 * Identity conversion
 * 
 * \param val Value to convert
 * \return The same value
 *
 * \ingroup core
 */
inline value
from(value val)
{ return val; }

/** \} */

/**
 * \name List constructor
 * \{
 */

/**
 * Dot type for dotted pairs in list construction
 * 
 * \ingroup core
 */
struct dot_t { };
constexpr dot_t dot; /**< Dot constant */

/**
 * Create a single-element list
 * 
 * \param head List element
 * \return List with a single element
 *
 * \ingroup core
 */
template <typename Head>
value
list(Head head)
{ return pair(from(head), nil); }

/**
 * Create a dotted pair
 * 
 * \param car First element
 * \param _ Dot marker (unused)
 * \param cdr Second element
 * \return Dotted pair
 *
 * \ingroup core
 */
template <typename Car, typename Cdr>
value
list(Car car, [[maybe_unused]] dot_t _, Cdr cdr)
{ return pair(from(car), from(cdr)); }

/**
 * Create a list from multiple elements
 * 
 * \param head First element
 * \param tail Rest of the elements
 * \return List containing all elements
 *
 * \ingroup core
 */
template <typename Head, typename ...Tail>
value
list(Head head, Tail&& ...tail)
{ return pair(from(head), list(std::forward<Tail>(tail)...)); }

/**
 * Reverse a list
 * 
 * \param l List to reverse
 * \return New list with elements in reverse order
 *
 * \ingroup core
 */
inline value
reverse(value l)
{
  value acc = nil;
  for (; l->t == tag::pair; l = value {&*l->cdr})
    acc = pair(value {&*l->car}, acc);
  return acc;
}

/**
 * Create a list from a range
 * 
 * \param range Range of elements
 * \return List containing all elements from the range
 */
template <std::ranges::range Range>
value
list(Range range)
{
  if constexpr (std::ranges::bidirectional_range<Range>)
  {
    value acc = nil;
    for (const value x : range | std::views::reverse)
      acc = pair(x, acc);
    return acc;
  }
  else
  {
    value acc = nil;
    for (const value x : range)
      acc = pair(x, acc);
    return reverse(acc);
  }
}

/** \} */

/**
 * \name Type tests
 * \{
 */

/**
 * Check if a value is a symbol
 * 
 * \param x Value to check
 * \return True if the value is a symbol
 *
 * \ingroup core
 */
inline bool
issym(value x)
{ return x->t == tag::sym; }

/**
 * Check if a value is a specific symbol
 * 
 * \param x Value to check
 * \param str Symbol name to compare with
 * \return True if the value is a symbol with the given name
 *
 * \ingroup core
 */
inline bool
issym(value x, std::string_view str)
{ return issym(x) and (str == std::string_view {x->sym.data, x->sym.len}); }

/**
 * Check if a value is a string
 * 
 * \param x Value to check
 * \return True if the value is a string
 *
 * \ingroup core
 */
inline bool
isstr(value x)
{ return x->t == tag::str; }

/**
 * Check if a value is a specific string
 * 
 * \param x Value to check
 * \param str String content to compare with
 * \return True if the value is a string with the given content
 *
 * \ingroup core
 */
inline bool
isstr(value x, const char *str)
{ return isstr(x) and strncmp(x->sym.data, str, x->sym.len) == 0; }

/**
 * Check if a value is a number
 * 
 * \param x Value to check
 * \return True if the value is a number
 *
 * \ingroup core
 */
inline bool
isnum(value x)
{ return x->t == tag::num; }

/**
 * Check if a value is a specific number
 * 
 * \param x Value to check
 * \param num Number to compare with
 * \return True if the value is a number equal to the given number
 *
 * \ingroup core
 */
inline bool
isnum(value x, long double num)
{ return isnum(x) and x->num == num; }

/** \} */

/**
 * \name Basic functions
 * \{
 */

/**
 * Check if two values are the same object
 * 
 * \param a First value
 * \param b Second value
 * \return True if the values are the same object
 *
 * \ingroup core
 */
inline bool
is(value a, value b)
{ return &*a == &*b; }

/**
 * Check if two values are equal
 * 
 * \param a First value
 * \param b Second value
 * \return True if the values are equal
 *
 * \ingroup core
 */
bool
equal(value a, value b);

/** \} */

/**
 * \name Basic list functions
 * \{
 */

/**
 * Create a pair (cons cell)
 * 
 * \param car First element
 * \param cdr Second element
 * \return Pair value
 *
 * \ingroup core
 */
inline value
cons(value car, value cdr)
{ return pair(car, cdr); }

/**
 * Get the first element of a pair
 * 
 * \tparam Test Whether to check if the value is a pair
 * \param x Pair value
 * \return First element of the pair
 * \throws std::runtime_error If the value is not a pair and Test is true
 *
 * \ingroup core
 */
template <bool Test=true>
inline value
car(value x)
{
  if constexpr (Test)
  {
    if (x->t != tag::pair)
      throw std::runtime_error {"car() - not a pair"};
  }
  return value {x->car};
}

/**
 * Get the second element of a pair
 * 
 * \tparam Test Whether to check if the value is a pair
 * \param x Pair value
 * \return Second element of the pair
 * \throws std::runtime_error If the value is not a pair and Test is true
 *
 * \ingroup core
 */
template <bool Test=true>
inline value
cdr(value x)
{
  if constexpr (Test)
  {
    if (x->t != tag::pair)
      throw std::runtime_error {"cdr() - not a pair"};
  }
  return value {x->cdr};
}

/**
 * Get the length of a list
 * 
 * \param l List value
 * \return Length of the list
 *
 * \ingroup core
 */
inline size_t
length(value l)
{
  size_t len = 0;
  for (; l->t == tag::pair; l = cdr(l), ++len);
  return len;
}

/**
 * Check if a value is a member of a list (using object identity)
 * 
 * \param x Value to check
 * \param l List to search in
 * \return True if the value is a member of the list
 *
 * \ingroup core
 */
inline bool
memq(value x, value l)
{
  for (; l->t == tag::pair; l = cdr(l))
  {
    if (is(x, car(l)))
      return true;
  }
  return false;
}

/**
 * Check if a value is a member of a list (using value equality)
 * 
 * \param x Value to check
 * \param l List to search in
 * \return True if the value is a member of the list
 *
 * \ingroup core
 */
inline bool
member(value x, value l)
{
  for (; l->t == tag::pair; l = cdr(l))
  {
    if (equal(x, car(l)))
      return true;
  }
  return false;
}

/**
 * Look up a key in an association list using object identity
 * 
 * \param k Key to look up
 * \param l Association list to search in
 * \param result Output parameter to store the value if found
 * \return True if the key was found, false otherwise
 * \throws std::runtime_error If the list is not an association list
 *
 * \ingroup core
 */
inline bool
assq(value k, value l, value &result)
{
  for (; l->t == tag::pair; l = cdr(l))
  {
    const value kv = car(l);
    if (kv->t != tag::pair)
      throw std::runtime_error {"assq() used on non-associative list"};
    if (is(k, car(kv)))
    {
      result = cdr(kv);
      return true;
    }
  }
  return false;
}

/**
 * Look up a key in an association list using value equality
 * 
 * \param k Key to look up
 * \param l Association list to search in
 * \param result Output parameter to store the value if found
 * \return True if the key was found, false otherwise
 * \throws std::runtime_error If the list is not an association list
 *
 * \ingroup core
 */
inline bool
assoc(value k, value l, value &result)
{
  for (; l->t == tag::pair; l = cdr(l))
  {
    const value kv = car(l);
    if (kv->t != tag::pair)
      throw std::runtime_error {"assoc() used on non-associative list"};
    if (equal(k, car(kv)))
    {
      result = cdr(kv);
      return true;
    }
  }
  return false;
}

/**
 * Append a value to the end of a list
 * 
 * \param l List to append to
 * \param x Value to append
 * \return New list with x appended
 *
 * \ingroup core
 */
inline value
append(value l, value x)
{
  if (l->t == tag::pair)
    return cons(car(l), append(cdr(l), x));
  else
    return x;
}

/** \} */

/**
 * \name Printing
 * \{
 */

 /**
  * Write value in a format that can be parsed back preserving the value
  *
  * \ingroup core
  */
void
write(std::ostream &os, const opi::value &val);

/**
 * Write value in a human appealing format
 *
 * \ingroup core
 */
void
display(std::ostream &os, const opi::value &val);

/**
 * Mixture of write- and display- formats
 *
 * \ingroup core
 */
void
print(std::ostream &os, const opi::value &val);

/** \} */

} // namespace opi


#include "opium/cons_list_view.inl" // IWYU pragma: export


/**
 * Output a value to a stream
 * 
 * \param os Output stream
 * \param val Value to output
 * \return Reference to the output stream
 * 
 * \ingroup core
 */
inline std::ostream&
operator << (std::ostream &os, const opi::value &val)
{ opi::print(os, val); return os; }

inline
opi::value::value(const char *sym)
: m_ptr {&*opi::sym(sym)}
{ }

inline bool
opi::value::operator == (opi::value other) const noexcept
{ return opi::equal(*this, other); }

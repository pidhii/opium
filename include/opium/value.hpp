#pragma once

#include "opium/memory.hpp"

#include <string>
#include <ostream>
#include <cstring>
#include <ranges>



namespace opi {


enum class tag {
  nil,
  sym,
  str,
  num,
  pair,
  boolean,
};

struct object;

class value {
  public:
  explicit
  value(object *ptr): m_ptr {ptr} { }

  constexpr object*
  operator -> () const noexcept
  { return m_ptr; }

  constexpr object&
  operator * () const noexcept
  { return *m_ptr; }

  bool
  operator == (opi::value other) const noexcept;

  private:
  object *m_ptr;
}; // class opi::value


struct object {
  // XXX unsafe
  object(tag tag): t {tag} { }

  tag t;
  union {
    struct { char *data; size_t len; } sym;
    struct { char *data; size_t len; } str;
    struct { object *car, *cdr; };
    long double num;
    bool boolean;
  };
}; // struct opi::object


////////////////////////////////////////////////////////////////////////////////
//
//                        Fundamental constructors
//
inline value
sym(const std::string &str)
{
  value ret {make<object>(tag::sym)};
  ret->sym.data = static_cast<char*>(allocate_atomic(str.length() + 1));
  std::memcpy(ret->sym.data, str.c_str(), str.length() + 1);
  ret->sym.len = str.length();
  return ret;
}

inline value
str(const std::string &str)
{
  value ret {make<object>(tag::str)};
  ret->str.data = static_cast<char*>(allocate_atomic(str.length() + 1));
  std::memcpy(ret->str.data, str.c_str(), str.length() + 1);
  ret->str.len = str.length();
  return ret;
}

inline value
num(long double val)
{
  value ret {make_atomic<object>(tag::num)};
  ret->num = val;
  return ret;
}

inline value
pair(value car, value cdr)
{
  value ret {make<object>(tag::pair)};
  ret->car = &*car;
  ret->cdr = &*cdr;
  return ret;
}

extern value True, False;
extern value nil;


////////////////////////////////////////////////////////////////////////////////
//
//                   Overloaded constructors from C++ types (casts)
//
inline value
from(const std::string &s)
{ return str(s); }

inline value
from(long double x)
{ return num(x); }

//inline value
//from(bool x)
//{ return boolean(x); }

inline value
from(const std::pair<value, value> &p)
{ return pair(p.first, p.second); }

inline value
from(value val)
{ return val; }


////////////////////////////////////////////////////////////////////////////////
//
//                            List constructor
//
struct dot_t { };
constexpr dot_t dot;

template <typename Head>
value
list(Head head)
{ return pair(from(head), nil); }

template <typename Car, typename Cdr>
value
list(Car car, [[maybe_unused]] dot_t _, Cdr cdr)
{ return pair(from(car), from(cdr)); }

template <typename Head, typename ...Tail>
value
list(Head head, Tail&& ...tail)
{ return pair(from(head), list(std::forward<Tail>(tail)...)); }

template <std::ranges::bidirectional_range Range>
value
list(Range range)
{
  value acc = nil;
  for (value x : range | std::views::reverse)
    acc = pair(x, acc);
  return acc;
}


////////////////////////////////////////////////////////////////////////////////
//
//                               Tests
//
inline bool
issym(value x)
{ return x->t == tag::sym; }

inline bool
issym(value x, const char *str)
{ return issym(x) and strncmp(x->sym.data, str, x->sym.len) == 0; }

inline bool
isstr(value x)
{ return x->t == tag::str; }

inline bool
isstr(value x, const char *str)
{ return isstr(x) and strncmp(x->sym.data, str, x->sym.len) == 0; }

inline bool
isnum(value x)
{ return x->t == tag::num; }

inline bool
isnum(value x, long double num)
{ return isnum(x) and x->num == num; }

////////////////////////////////////////////////////////////////////////////////
//
//                            Basic functions
//
inline bool
is(value a, value b)
{ return &*a == &*b; }

inline bool
equal(value a, value b)
{
  if (is(a, b))
    return true;

  if (a->t != b->t)
    return false;

  switch (a->t)
  {
    case tag::sym:
      return a->sym.len == b->sym.len and
             std::strncmp(a->sym.data, b->sym.data, a->sym.len) == 0;

    case tag::nil:
      return true;

    case tag::num:
      return a->num == b->num;

    case tag::str:
      return a->str.len == b->str.len and
             std::strncmp(a->str.data, b->str.data, a->str.len) == 0;

    case tag::pair:
      return equal(value {a->car}, value {b->car}) and
             equal(value {a->cdr}, value {b->cdr});

    case tag::boolean:
      return a->boolean == b->boolean;
  }

  abort();
}

////////////////////////////////////////////////////////////////////////////////
//
//                          Basic list functions
//
inline value
cons(value car, value cdr)
{ return pair(car, cdr); }

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

inline size_t
length(value l)
{
  size_t len = 0;
  for (; l->t == tag::pair; l = cdr(l), ++len);
  return len;
}


////////////////////////////////////////////////////////////////////////////////
//
//                             Utility
//
struct alist_iterator {
  using difference_type = ptrdiff_t;

  explicit
  alist_iterator(value l): m_l {l} { }

  value
  operator * () const noexcept
  { return car<false>(m_l); }

  alist_iterator&
  operator ++ () noexcept
  { m_l = cdr<false>(m_l); return *this; }

  alist_iterator
  operator ++ (int) noexcept
  { return alist_iterator {cdr(m_l)}; }

  private:
  value m_l;

  friend struct alist_sentinel;
};

struct alist_sentinel {
  bool
  operator == (const alist_iterator &it) const noexcept
  { return it.m_l->t != tag::pair; }
};

inline auto
range(value l)
{ return std::ranges::subrange(alist_iterator {l}, alist_sentinel {}); }


} // namespace opi



std::ostream&
operator << (std::ostream &os, const opi::value &val);

inline bool
opi::value::operator == (opi::value other) const noexcept
{ return opi::equal(*this, other); }


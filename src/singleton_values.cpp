#include "opium/value.hpp"


////////////////////////////////////////////////////////////////////////////////
//
//                             Booleans
//
static opi::object*
_initialize_boolean(opi::object *ptr, bool val)
{
  ptr->t = opi::tag::boolean;
  ptr->boolean = val;
  return ptr;
}

static
opi::object True_object {opi::tag::boolean}, False_object {opi::tag::boolean};
const opi::value opi::True {_initialize_boolean(&True_object, true)},
                opi::False {_initialize_boolean(&False_object, false)};

////////////////////////////////////////////////////////////////////////////////
//
//                              Nil
//
static
opi::object nil_object {opi::tag::nil};
const opi::value opi::nil {&nil_object};

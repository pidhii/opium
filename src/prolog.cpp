#include "opium/prolog.hpp"
#include "opium/value.hpp"
#include <regex.h>


const opi::predicate&
opi::prolog::add_predicate(const predicate &pred)
{ return m_db.emplace(pred.name(), pred)->second; }

const opi::predicate&
opi::prolog::add_predicate(value sig, value body)
{ return add_predicate(predicate {sig, body}); }

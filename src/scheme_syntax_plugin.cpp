#include "opium/scheme/scheme_transformations.hpp"


opi::stl::list<opi::scheme_syntax_plugin*> opi::scheme_syntax_plugin::m_plugins;


opi::scheme_syntax_plugin::scheme_syntax_plugin()
{ m_plugins.push_back(this); }


opi::scheme_syntax_plugin::~scheme_syntax_plugin()
{ m_plugins.erase(std::find(m_plugins.begin(), m_plugins.end(), this)); }

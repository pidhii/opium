#include "opium/scheme/scheme_transformations.hpp"


static struct: opi::scheme_syntax_plugin {
  using opi::scheme_syntax_plugin::scheme_syntax_plugin;

  void
  load(opi::scheme_code_flattener &t)
  {

  }

  void
  load(opi::scheme_unique_identifiers &t)
  {

  }

  void
  load(opi::scheme_to_prolog &t)
  {
    using namespace opi;

    // <+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>
    //                             parameterize
    const match parameterize {list("parameterize"),
                              list("parameterize", "params", dot, "body")};
    t.append_rule(parameterize, [&t](const auto &ms) {
      const value params = ms.at("params");
      const value body = ms.at("body");
      assert(islist(params));
      assert(islist(body));

      stl::vector<value> code;
      for (const value p : range(params))
      {
        const value pname = list_ref(p, 0);
        const value pval = list_ref(p, 1);
        const value ptype = t.to_type(pname, true, std::back_inserter(code));
        assert(car(ptype) == "parameter");
        const value etype = t.to_type(pval, true, std::back_inserter(code));
        const value testp = list("=", ptype, etype);
        code.push_back(testp);
      }

      std::ranges::copy(range(t.transform_block(body)), std::back_inserter(code));

      return cons("and", list(code));
    });
  }
} parameterize;

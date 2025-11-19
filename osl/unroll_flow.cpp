#include "unroll_flow.hpp"

#include "opium/exceptions.hpp"
#include "opium/stl/vector.hpp"
#include "opium/value.hpp"


std::pair<opi::osl::flow, opi::value>
opi::osl::flow_unroller::unroll(value expr, value next) const
{
  if (ispair(expr))
  {
    const value form = car(expr);
    const value args = cdr(expr);
    if (form == "if")
    {
      if (length(args) == 3)
      {
        const value cond = car(args);
        const value thenbr = car(cdr(args));
        const value elsebr = car(cdr(cdr(args)));

        const auto [thenflow, newthenbr] = unroll(thenbr, next);
        const auto [elseflow, newelsebr] = unroll(elsebr, next);

        if (thenflow == flow::keep and elseflow == flow::keep)
        {
          return {flow::keep, list("if", cond, newthenbr, newelsebr)};
        }
        else if (thenflow == flow::keep and elseflow == flow::branch)
        {
          const value newnext = unroll_block(next, nil, nil).second;
          const value thenbr = list("begin", newthenbr, dot, newnext);
          return {flow::branch, list("if", cond, thenbr, newelsebr)};
        }
        else if (thenflow == flow::branch and elseflow == flow::keep)
        {
          const value newnext = unroll_block(next, nil, nil).second;
          const value elsebr = list("begin", newelsebr, dot, newnext);
          return {flow::branch, list("if", cond, newthenbr, elsebr)};
        }
        else // branch and branch
        {
          return {flow::branch, list("if", cond, newthenbr, newelsebr)};
        }
      }
      else if (length(args) == 2)
      {
        const value cond = car(args);
        const value thenbr = car(cdr(args));

        const auto [thenflow, newthenbr] = unroll(thenbr, next);

        if (thenflow == flow::keep)
          return {flow::keep, list("if", cond, newthenbr)};
        else // branch
        {
          const value elsebr = unroll_block(next, nil, nil).second;
          return {flow::branch, list("if", cond, newthenbr, cons("begin", elsebr))};
        }
      }
      else
        throw bad_code {"Invalid if-expression", expr};
    } // if (from == "if")
    else if (form == "begin")
    {
      const auto [flow, newblock] = unroll_block(args, next, nil);
      return {flow, cons("begin", newblock)};
    }
    else if (form == "let" or form == "let*" or form == "let-values" or
             form == "let*-values" or form == "letrec" or form == "letrec*")
    {
      const value bindings = car(args);
      const value body = cdr(args);

      const auto [flow, newbody] = unroll_block(body, next, nil);
      return {flow, list(form, bindings, dot, newbody)};
    }
    else if (form == "cases")
    {
      const value exprs = car(args);
      const value clauses = cdr(args);
      stl::vector<std::pair<flow, value>> branchunrolls;
      // Unroll branches and figure out resultant flow
      flow resflow = flow::keep;
      for (const value clause : range(clauses))
      {
        const value pats = car(clause);
        const value body = cdr(clause);
        const auto [flow, newbody] = unroll_block(body, next, nil);
        if (flow == flow::branch)
          resflow = flow::branch;
        branchunrolls.emplace_back(flow, cons(pats, newbody));
      }
      // Handle the flow
      const auto second = [](const auto &pair) { return pair.second; };
      value newclauses;
      if (resflow == flow::keep)
      {
        newclauses = list(std::views::transform(branchunrolls, second));
      }
      else // branch
      {
        // fix branches that were 'kept'
        const value newnext = unroll_block(next, nil, nil).second;
        for (const auto &[flow, clause] : branchunrolls)
        {
          if (flow == flow::keep)
          {
            const value pats = car(clause);
            const value body = cdr(clause);
            const value newbody = append(body, newnext);
            const value newclause = cons(pats, newbody);
            append_mut(newclauses, list(newclause));
          }
          else
            append_mut(newclauses, list(clause));
        }
      }
      return {resflow, list("cases", exprs, dot, newclauses)};
    }
    else if (issym(form, m_return_name))
    {
      return make_return(args);
    }
  }

  // nothing else impacts the flow
  return {flow::keep, expr};
}


std::pair<opi::osl::flow, opi::value>
opi::osl::flow_unroller::unroll_block(value exprs, value next, value acc) const
{
  if (ispair(exprs))
  {
    const value head = car(exprs);
    const value tail = cdr(exprs);

    const value newnext = append(tail, next);
    const auto [flow, newhead] = unroll(head, newnext);

    if (flow == flow::keep)
      return unroll_block(tail, next, cons(newhead, acc));
    else // branch
      return {flow::branch, reverse(cons(newhead, acc))};
  }
  else // no more exprs
    return {flow::keep, reverse(acc)};
}

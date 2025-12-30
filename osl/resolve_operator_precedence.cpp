#include "operators.hpp"
#include "opium/scheme/scheme_code_transformer.hpp"
#include "opium/lisp_parser.hpp"

#include <stdexcept>


namespace opi::osl::detail {


struct resolver_node {
  enum class kind { expr, sufix, infix, postfix } kind;
  union {
    value expr;
    struct { operator_definition op; resolver_node *arg; } unary;
    struct { operator_definition op; resolver_node *lhsarg, *rhsarg; } infix;
  };

  resolver_node(): kind {kind::expr} {}
}; // struct opi::osl::detail::resolver_node


static int
node_precedence(const resolver_node *node)
{
  switch (node->kind)
  {
    case resolver_node::kind::expr:
      return 0;
    
    case resolver_node::kind::sufix:
    case resolver_node::kind::postfix:
      return node->unary.op.precedence;

    case resolver_node::kind::infix:
      return node->infix.op.precedence;
  }

  std::terminate();
}


enum class direction { left, right };

template <direction Direction>
resolver_node*&
get_operator_argument(resolver_node *node)
{
  switch (node->kind)
  {
    case resolver_node::kind::sufix:
    case resolver_node::kind::postfix:
      return node->unary.arg;

    case resolver_node::kind::infix:
      if constexpr (Direction == direction::left)
        return node->infix.lhsarg;
      else
        return node->infix.rhsarg;

    default:
      throw std::logic_error {"Non-operator node"};
  }
}

template <direction Direction>
void
set_operator_argument(resolver_node *node, resolver_node *arg)
{ get_operator_argument<Direction>(node) = arg; }


template <direction Direction>
resolver_node*
rotate_nodes(resolver_node *root)
{
  if constexpr (Direction == direction::right)
  { // (A -> x) <- B  >>  A -> (x <- B)
    resolver_node *bnode = root;
    resolver_node *anode = get_operator_argument<direction::left>(bnode);
    resolver_node *xnode = get_operator_argument<direction::right>(anode);
    set_operator_argument<direction::left>(bnode, xnode);
    set_operator_argument<direction::right>(anode, bnode);
    return anode;
  }

  if constexpr (Direction == direction::left)
  { // A -> (x <- B)  >>  (A -> x) <- B
    resolver_node *anode = root;
    resolver_node *bnode = get_operator_argument<direction::right>(anode);
    resolver_node *xnode = get_operator_argument<direction::left>(bnode);
    set_operator_argument<direction::right>(anode, xnode);
    set_operator_argument<direction::left>(bnode, anode);
    return bnode;
  }
}


static resolver_node*
balance_tree(resolver_node *node)
{
  switch (node->kind)
  {
    case resolver_node::kind::expr:
      // Don't touch (normalized) expressions
      return node;

    case resolver_node::kind::sufix:
    {
      const int opprec = node->unary.op.precedence;
      switch (node->unary.arg->kind)
      {
        case resolver_node::kind::expr:
          return node;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // S -> (S -> ?)
        case resolver_node::kind::sufix:
          break;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // S -> (? <- P)
        // S -> (? <- I)
        //
        case resolver_node::kind::postfix:
        case resolver_node::kind::infix:
        {
          const int argprec = node_precedence(node->unary.arg);
          if (abs(argprec) > opprec)
          {
            resolver_node *newroot = rotate_nodes<direction::left>(node);
            return balance_tree(newroot);
          }
          else if (abs(argprec) == opprec)
            throw ambiguous_operator_use {
                "Ambiguous sufix-postfix/infix operators", node->unary.op.name};
          // else abs(argprec) < opprec => correct

          break;
        }
      }

      resolver_node *newarg = balance_tree(node->unary.arg);
      if (newarg != node->unary.arg)
        return balance_tree(node);
      else
        return node;
    } // case op is sufix

    case resolver_node::kind::postfix:
    {
      const int opprec = node->unary.op.precedence;
      switch (node->unary.arg->kind)
      {
        case resolver_node::kind::expr:
          return node;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // (? <- P) <- P
        //
        case resolver_node::kind::postfix:
          break;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // (S -> ?) <- P
        // (I -> ?) <- P
        //
        case resolver_node::kind::sufix:
        case resolver_node::kind::infix:
        {
          const int argprec = node_precedence(node->unary.arg);
          if (abs(argprec) > opprec)
          {
            resolver_node *newroot = rotate_nodes<direction::right>(node);
            return balance_tree(newroot);
          }
          else if (abs(argprec) == opprec)
            throw ambiguous_operator_use {
                "Ambiguous sufix/infix-postfix operators", node->unary.op.name};
          // else abs(argprec) < opprec => correct

          break;
        }
      }

      resolver_node *newarg = balance_tree(node->unary.arg);
      if (newarg != node->unary.arg)
        return balance_tree(node);
      else
        return node;
    } // case op is prefix

    case resolver_node::kind::infix:
    {
      const int opprec = node->infix.op.precedence;
      const int lhsprec = node_precedence(node->infix.lhsarg);
      const int rhsprec = node_precedence(node->infix.rhsarg);
      resolver_node *lhs = node->infix.lhsarg;
      resolver_node *rhs = node->infix.rhsarg;

      // o Handle LHS (if necessary)
      switch (lhs->kind)
      {
        // (P -> x) <- I   >>   ignore
        case resolver_node::kind::postfix:
        case resolver_node::kind::expr:
          // ignore
          break;

        // (S -> x) <- I
        case resolver_node::kind::sufix:
        {
          if (lhsprec == abs(opprec))
            throw ambiguous_operator_use {"Ambiguous sufix-infix operators",
                                          node->infix.op.name};
          else if (lhsprec > abs(opprec))
          {
            resolver_node *newroot = rotate_nodes<direction::right>(node);
            return balance_tree(newroot);
          }
          // else lhsprec < abs(opprec) => correct

          break;
        }

        // (x <- I -> y) <- I
        case resolver_node::kind::infix:
        {
          if (abs(lhsprec) == abs(opprec) and lhsprec*opprec < 0)
            throw ambiguous_operator_use {"Ambiguous infix-infix operators",
                                          node->infix.op.name};
          if (abs(lhsprec) == abs(opprec))
          {
            if (opprec > 0)
            {
              resolver_node *newroot = rotate_nodes<direction::right>(node);
              return balance_tree(newroot);
            }
            // else opprec < 0 => correct
          }
          else if (abs(lhsprec) > abs(opprec))
          {
            resolver_node* newroot = rotate_nodes<direction::right>(node);
            return balance_tree(newroot);
          }
          // else abs(lhsprec) < abs(opprec) => correct
        
          break;
        }
      }

      // Handle RHS (if necessary)
      switch (rhs->kind)
      {
        // I -> (S -> x)  >>   ignore
        case resolver_node::kind::sufix:
        case resolver_node::kind::expr:
          // ignore
          break;

        // I -> (P -> x)
        case resolver_node::kind::postfix:
        {
          if (rhsprec == abs(opprec))
            throw ambiguous_operator_use {"Ambiguous infix-postfix operators",
                                          node->infix.op.name};
          else if (rhsprec > abs(opprec))
          {
            resolver_node *newroot = rotate_nodes<direction::left>(node);
            return balance_tree(newroot);
          }
          // else rhsprec < abs(opprec) => correct

          break;
        }

        // I -> (x <- I -> y)
        case resolver_node::kind::infix:
        {
          if (abs(rhsprec) == abs(opprec) and rhsprec*opprec < 0)
            throw ambiguous_operator_use {"Ambiguous infix-infix operators",
                                          node->infix.op.name};
          if (abs(rhsprec) == abs(opprec))
          {
            if (opprec < 0)
            {
              resolver_node *newroot = rotate_nodes<direction::left>(node);
              return balance_tree(newroot);
            }
            // else opprec > 0 => correct
          }
          else if (abs(rhsprec) > abs(opprec))
          {
            resolver_node* newroot = rotate_nodes<direction::left>(node);
            return balance_tree(newroot);
          }
          // else abs(rhsprec) < abs(opprec) => correct
        
          break;
        }
      }

      // Balance LHS and RHS
      resolver_node *newlhs = balance_tree(node->infix.lhsarg);
      resolver_node *newrhs = balance_tree(node->infix.rhsarg);
      if (newlhs != node->infix.lhsarg or newrhs != node->infix.rhsarg)
        return balance_tree(node);
      else
        return node;
    } // case op is infix
  }

  std::terminate();
}


struct precedence_tree_builder {
  precedence_tree_builder(const operators_library &oplib): m_oplib {oplib} { }

  resolver_node*
  build(value expr)
  {
    if (ispair(expr))
    {
      if (car(expr) == "unresolved-sufix")
      {
        const value opname = list_ref(expr, 1);
        const value oparg = list_ref(expr, 2);
        resolver_node *node = make<resolver_node>();
        node->kind = resolver_node::kind::sufix;
        if (not m_oplib.find_operator(operator_kind::sufix, opname, node->unary.op))
          throw bad_code {std::format("No such sifix operator, {}", opname),
                          opname};
        node->unary.arg = build(oparg);
        return node;
      }

      if (car(expr) == "unresolved-postfix")
      {
        const value opname = list_ref(expr, 1);
        const value oparg = list_ref(expr, 2);
        resolver_node *node = make<resolver_node>();
        node->kind = resolver_node::kind::postfix;
        if (not m_oplib.find_operator(operator_kind::postfix, opname, node->unary.op))
          throw bad_code {std::format("No such postfix operator, {}", opname),
                          opname};
        node->unary.arg = build(oparg);
        return node;
      }

      if (car(expr) == "unresolved-infix")
      {
        const value opname = list_ref(expr, 1);
        const value lhsarg = list_ref(expr, 2);
        const value rhsarg = list_ref(expr, 3);
        resolver_node *node = make<resolver_node>();
        node->kind = resolver_node::kind::infix;
        if (not m_oplib.find_operator(operator_kind::infix, opname, node->infix.op))
          throw bad_code {std::format("No such infix operator, {}", opname),
                          opname};
        node->infix.lhsarg = build(lhsarg);
        node->infix.rhsarg = build(rhsarg);
        return node;
      }
    }

    resolver_node *node = make<resolver_node>();
    node->kind = resolver_node::kind::expr;
    node->expr = expr;
    return node;
  }

  private:
  const operators_library &m_oplib;
}; // struct opi::osl::detail::precedence_tree_builder


static opi::value
unfold_tree(const resolver_node *root)
{
  switch (root->kind)
  {
    case resolver_node::kind::expr:
      return root->expr;

    case resolver_node::kind::sufix:
    case resolver_node::kind::postfix:
    {
      const value op = root->unary.op.name;
      const value arg = unfold_tree(root->unary.arg);
      return list(op, arg);
    }

    case resolver_node::kind::infix:
    {
      const value op = root->infix.op.name;
      const value lhs = unfold_tree(root->infix.lhsarg);
      const value rhs = unfold_tree(root->infix.rhsarg);
      return list(op, lhs, rhs);
    }
  }

  std::terminate();
}


struct operator_precedence_resolver: ext_scheme_code_transformer {
  operator_precedence_resolver(const operators_library &oplib)
  : m_tree_builder {oplib}
  {
    const auto balance_handle = [this]([[maybe_unused]] const auto &_, value fm) {
      resolver_node *tree = m_tree_builder.build(fm);
      resolver_node *newtree = balance_tree(tree);
      return unfold_tree(newtree);
    };

    append_rule({"(unresolved-sufix)"_lisp, "(unresolved-sufix . _)"_lisp},
                balance_handle);
    append_rule({"(unresolved-postfix)"_lisp, "(unresolved-postfix . _)"_lisp},
                balance_handle);
    append_rule({"(unresolved-infix)"_lisp, "(unresolved-infix . _)"_lisp},
                balance_handle);

    append_rule({nil, "(f . xs)"_lisp}, [this]([[maybe_unused]] const auto &_, value fm) {
      return list(range(fm) | std::views::transform(std::ref(*this)));
    });

    append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
  }

  private:
  precedence_tree_builder m_tree_builder;
}; // struct opi::osl::detail::operator_precedence_resolver

} // namespace opi::osl::detail


opi::value
opi::osl::resolve_operator_precedence(value expr, const operators_library &oplib)
{
  detail::operator_precedence_resolver resolver {oplib};
  return resolver(expr);
}

/*
 * Opium - Ultimate static type system for type-annotation-free code
 * Copyright (C) 2025  Ivan Pidhurskyi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#include "opium/code_transform_utils.hpp"
#include "opium/code_transformer.hpp"
#include "opium/match.hpp"
#include "opium/scheme/scheme_transformations.hpp"
#include "opium/value.hpp"
#include "opium/utilities/ranges.hpp"

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <ranges>
#include <stdexcept>


static opi::value
pattern_constructor(opi::value pattern) noexcept
{
  if (issym(pattern))
    return "_";
  else
    return car(pattern);
}


static opi::value
pattern_arguments(opi::value pattern) noexcept
{
  if (issym(pattern))
    return pattern;
  else
    return cdr(pattern);
}


static size_t
pattern_arity(opi::value pattern) noexcept
{
  if (issym(pattern))
    return 1;
  else
    return length(cdr(pattern));
}


// TODO: optimize by removing copying by value
struct match_table {

  struct branch {
    enum class kind { sub_table, code } kind;

    branch(): kind {kind::code}, m_sub_table {nullptr} { }

    // ~branch() { if (kind == kind::sub_table) sub_table->~match_table(); }

    const match_table&
    table() const noexcept
    {
      assert(kind == kind::sub_table);
      return *m_sub_table;
    }

    opi::value
    code() const noexcept
    {
      assert(kind == kind::code);
      return m_code;
    }

    void
    set_table(match_table *table)
    {
      kind = kind::sub_table;
      m_sub_table = table;
    }

    void
    set_code(opi::value code) noexcept
    {
      kind = kind::code;
      m_code = code;
    }

    private:
    match_table *m_sub_table;
    opi::value m_code;
  };

  struct table_row {
    opi::stl::vector<opi::value> row_patterns;
    branch row_branch;

    opi::value&
    operator [] (size_t col) noexcept
    { return row_patterns.at(col); }

    const opi::value&
    operator [] (size_t col) const noexcept
    { return row_patterns.at(col); }
  };

  template <std::input_iterator RowIter>
    requires std::convertible_to<typename RowIter::value_type, table_row>
  match_table(RowIter begin, RowIter end)
  : table_rows {begin, end}
  { }

  size_t
  rows() const noexcept
  { return table_rows.size(); }

  size_t
  columns() const noexcept
  { return table_rows.empty() ? 0 : table_rows[0].row_patterns.size(); }

  table_row&
  operator [] (size_t row) noexcept
  { return table_rows.at(row); }

  const table_row&
  operator [] (size_t row) const noexcept
  { return table_rows.at(row); }

  opi::stl::vector<opi::value> table_column_inputs;
  opi::stl::vector<table_row> table_rows;
};


static void
print_table(std::ostream &os, const match_table &table, size_t indent = 0)
{
  for (size_t row = 0; row < table.rows(); ++row)
  {
    os << std::string(indent, ' ');
    os << std::format("[{:2}]", row);
    for (size_t col = 0; col < table.columns(); ++col)
    {
      opi::value input = opi::nil;
      try { input = table.table_column_inputs.at(col); }
      catch (...) { input = "???"; }
      os << "\t" << std::format("{}={}", input, table[row][col]);
    }

    const match_table::branch &branch = table[row].row_branch;
    switch (branch.kind)
    {
      case match_table::branch::kind::sub_table:
        os << "\t-> SUBTABLE:\n";
        print_table(os, branch.table(), indent+2);
        break;

      case match_table::branch::kind::code:
        os << "\t-> " << branch.code() << std::endl;
        break;
    }
  }
}


struct column_stats {
  opi::stl::unordered_map<opi::value, size_t> counts;

  size_t
  distinct_patterns() const noexcept
  { return counts.size(); }
};


static bool
is_normal_form(const column_stats &stats) noexcept
{
  const auto eq_1 = [](size_t x) { return x == 1; };
  const bool nodups = std::ranges::all_of(stats.counts | std::views::values, eq_1);
  return nodups;
}


static bool
is_wildcard(const column_stats &stats) noexcept
{
  const auto is_wild = [](opi::value x) { return x == "_"; };
  const bool allwild = std::ranges::all_of(stats.counts | std::views::keys, is_wild);
  return allwild;
}


static column_stats
analyse_column(const match_table &table, size_t col)
{
  column_stats stats;
  for (size_t row = 0; row < table.rows(); ++row)
  {
    const opi::value pattern = table[row][col];
    if (opi::ispair(pattern))
      stats.counts[car(pattern)] += 1;
    else
     stats.counts["_"] += 1;
  }
  return stats;
}


static opi::stl::vector<column_stats>
analyse_table(const match_table &table)
{
  opi::stl::vector<column_stats> result;
  for (size_t col = 0; col < table.columns(); ++col)
    result.emplace_back(analyse_column(table, col));
  return result;
}


static void
sort(match_table &table, size_t col)
{
  opi::stl::unordered_map<opi::value, size_t> ctorseq;
  for (size_t row = 0; row < table.rows(); ++row)
  {
    const opi::value pattern = table[row][col];
    const opi::value ctor = pattern_constructor(pattern);
    ctorseq.emplace(ctor, row);
  }

  using table_row = match_table::table_row;
  const auto cmp = [&](const table_row &a, const table_row &b) {
    const opi::value actor = pattern_constructor(a[col]);
    const opi::value bctor = pattern_constructor(b[col]);
    return ctorseq[actor] < ctorseq[bctor];
  };
  std::stable_sort(table.table_rows.begin(), table.table_rows.end(), cmp);
}


static match_table
simplify_table(match_table table, opi::symbol_generator &gensym);

static match_table
squash_column(match_table &table, size_t col, opi::symbol_generator &gensym)
{
  opi::stl::list<opi::stl::list<match_table::table_row>> partition;

  // Parition table on distinct predicates
  sort(table, col);
  for (size_t row = 0; row < table.rows(); ++row)
  {
    const match_table::table_row &currow = table[row];
    const opi::value pattern = currow[col];
    const opi::value ctor = pattern_constructor(pattern);

    if (partition.empty())
    {
      partition.emplace_back().emplace_back(table[row]);
      continue;
    }

    opi::stl::list<match_table::table_row> &lastpart = partition.back();
    if (ctor == pattern_constructor(lastpart.front()[col]))
      lastpart.emplace_back(currow);
    else
      partition.emplace_back().emplace_back(table[row]);
  }

  const auto issym = [](opi::value x) { return opi::issym(x); };

  opi::stl::list<match_table::table_row> newtable;
  for (opi::stl::list<match_table::table_row> &part : partition)
  {
    assert(part.size() > 0);

    const opi::value refpattern = part.front()[col];
    const opi::value ctor = pattern_constructor(refpattern);
    const opi::value args = pattern_arguments(refpattern);


    if (part.size() == 1 and std::ranges::all_of(range(args), issym))
    { // Nothing to squash, save the row intact
      const match_table::table_row singlerow = part.front();
      newtable.emplace_back(singlerow);
    }

    else
    { // Squash rows
      const size_t arity = pattern_arity(refpattern);
      const size_t oldcolumns = part.front().row_patterns.size();

      // Create rows for new (sub)table
      for (match_table::table_row &newrow : part)
      {
        // Expand the pattern in place of the subject pattern
        const opi::value pattern = newrow[col];
        newrow.row_patterns.erase(newrow.row_patterns.begin() + col);
        for (const opi::value p : range(reverse(pattern_arguments(pattern))))
          newrow.row_patterns.insert(newrow.row_patterns.begin() + col, p);
      }

      // Identifiers for the pattern in the squashed row
      opi::stl::vector<opi::value> argidents;
      std::ranges::generate_n(std::back_inserter(argidents), arity, gensym);

      // Sbustitute for the squshed rows to put into the new table
      match_table::table_row squashedrow;
      squashedrow.row_patterns.resize(oldcolumns, opi::nil);
      opi::stl::vector<opi::value> inputs;
      for (size_t i = 0; i < oldcolumns; ++i)
      {
        if (i == col)
        {
          squashedrow.row_patterns[i] = cons(ctor, list(argidents));
          std::ranges::copy(argidents, std::back_inserter(inputs));
        }
        else
        {
          const opi::value newident = gensym();
          squashedrow.row_patterns[i] = newident;
          inputs.push_back(newident);
        }
      }

      match_table subtable = simplify_table({part.begin(), part.end()}, gensym);
      subtable.table_column_inputs = inputs;
      squashedrow.row_branch.set_table(opi::make<match_table>(std::move(subtable)));
      newtable.emplace_back(squashedrow);
    }
  }

  match_table result {newtable.begin(), newtable.end()};
  result.table_column_inputs = table.table_column_inputs;
  return result;
}


static bool
is_flat_pattern(opi::value pattern)
{
  const auto issym = [](opi::value x) { return opi::issym(x); };

  if (issym(pattern))
    return true;
  else
    return std::ranges::all_of(range(cdr(pattern)), issym);
}


static match_table
simplify_table(match_table table, opi::symbol_generator &gensym)
{
  const opi::stl::vector<column_stats> stats = analyse_table(table);

  // Check if table contains a column in the normal form
  const auto it = std::ranges::find_if(stats, is_normal_form);
  if (it != stats.end())
  { // Found normal form column, maybe we are done, but:
    // Check if all subpatterns are wildcards
    // yes => nothing to simplify
    //  no => squash corresponding column and check again
    for (size_t row = 0; row < table.rows(); ++row)
    {
      for (size_t col = 0; col < table.columns(); ++col)
      {
        if (not is_flat_pattern(table[row][col]))
        {
          // Fix the column
          match_table fixedtable = squash_column(table, col, gensym);
          // And simplify again (in case there is anything to)
          return simplify_table(fixedtable, gensym);
        }
      }
    }

    // All done
    return table;
  }

  // None of the columns is in normal form => create one by squashing any column
  // NOTE: squashing wildcard column doesn not actually simplify anything, so
  //       it is necessary to find a non-wildcard column
  // TODO: use some heuristic for more optimal choise of column to squash
  const auto nonwild = std::ranges::find_if_not(stats, is_wildcard);
  if (nonwild == stats.end())
    throw std::invalid_argument {"Invalid match table (2)"};

  const size_t squashcol = std::distance(stats.begin(), nonwild);
  return squash_column(table, squashcol, gensym);
}


static opi::value
build_cases(const match_table &table)
{
  const opi::stl::vector<column_stats> stats = analyse_table(table);

  // Build cases
  opi::value cases = opi::nil;
  for (size_t row = 0; row < table.rows(); ++row)
  {
    const opi::value patterns = list(table[row].row_patterns);

    opi::value branch = opi::nil;
    const match_table::branch &br = table[row].row_branch;
    switch (br.kind)
    {
      case match_table::branch::kind::sub_table:
        branch = list(build_cases(br.table())); // branch must be a block-expression
        break;

      case match_table::branch::kind::code:
        branch = br.code();
        break;
    }

    cases = append(cases, list(cons(patterns, branch)));
  }

  return list("cases", list(table.table_column_inputs), opi::dot, cases);
}


opi::scheme_code_flattener::scheme_code_flattener(symbol_generator &gensym)
: m_gensym {gensym}
{
  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                             annotate-type
  const match asstypematch {list("annotate-type"),
                            list("annotate-type", "expr", "type")};
  append_rule(asstypematch, [this](const auto &ms) {
    const value expr = ms.at("expr");
    const value type = ms.at("type");

    // Only flatten the expr
    if (ispair(expr))
    {
      const value uid = m_gensym();
      const value newexpr = (*this)(expr);
      return list("let", list(list(uid, newexpr)), list("annotate-type", uid, type));
    }
    else // Already flat
      return list("annotate-type", expr, type);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                cases
  const match casesmatch {
      list("cases"), list("cases", "exprs", cons("patterns", "branch"), "...")};
  prepend_rule(casesmatch, [this](const auto &ms, value) {
    const value exprs = ms.at("exprs");
    const value patterns = ms.contains("patterns") ? ms.at("patterns") : nil;
    const value branches = ms.contains("branch") ? ms.at("branch") : nil;

    opi::stl::vector<match_table::table_row> rows;
    for (auto [rowpatterns, branch] :
         utl::zip(range(patterns), range(branches)))
    {
      match_table::table_row &newrow = rows.emplace_back();
      std::ranges::copy(range(rowpatterns), std::back_inserter(newrow.row_patterns));
      newrow.row_branch.set_code(transform_block(*this, branch));
    }

    // Create identifiers for all expressions
    opi::stl::vector<value> expridents;
    const auto issym = [](opi::value x) { return opi::issym(x); };
    for (const value expr : range(exprs))
    {
      const value exprident = issym(expr) ? expr : m_gensym();
      expridents.push_back(exprident);
    }

    match_table table {rows.begin(), rows.end()};
    table.table_column_inputs = expridents;
    table = simplify_table(table, m_gensym);

    const value newcases = build_cases(table); // TODO
    // std::cout << "resulting cases:\n" << pprint_scm(newcases);
    // std::cout << std::endl;

    // If all expressions are symbols, return the cases directly
    // Otherwise, bind non-symbol expressions to temporary variables
    if (std::ranges::all_of(range(exprs), issym))
      return newcases;
    else
    {
      // Create bindings for all non-symbol expressions
      value bindings = nil;
      for (const auto [expr, exprident] : utl::zip(range(exprs), expridents))
      {
        if (not issym(expr)) 
          bindings = append(bindings, list(list(exprident, (*this)(expr))));
      }
      return list("let", bindings, newcases);
    }
  });


  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                                set!
  // NOTE: This handler is not necessary and can be removed; however, the
  // following could happen in that case:
  // - there LHS of the set! form is some compound expression (invalid code)
  // - handler of (for example) forms will lift it into a proxy variable
  // - consequently, resulting set! form will look totaly valid, except that the
  //   LHS is a temporary variable that the initial code has nothing to do with
  const match setmatch {list("set!"), list("set!", "ident", "expr")};
  prepend_rule(setmatch, [this](const auto &ms, value fm) {
    const value ident = ms.at("ident");
    const value expr = ms.at("expr");

    if (not issym("ident"))
    {
      throw bad_code {
          std::format("Invalid set! expression: expected identifier, got {}",
                      ident),
          fm};
    }

    // Flatten the RHS expression
    const value newexpr = (*this)(expr);
    return list("set!", ident, newexpr);
  });

  // <<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>><<+>>
  //                           IMPORT PLUGINS
  scheme_syntax_plugin::apply_all(*this);

  // function invocation
  // -------------------
  // Pull output nested comound expressions from inside the invocation by
  // separating them into bindings to temporary variables to replace
  // corresponding expressions in the invocation form:
  //
  // (<expr1> <expr2> ...) ->
  // (let ((<tmp1> T[<expr1>])
  //       (<tmp2> T[<expr2>])
  //             ...          )
  //   (<tmp1> <tmp2> ...))
  //
  append_rule({nil, list("form", "...")}, [this](const auto &ms, value fm) {
    value binds = nil;
    value result = nil;
    for (const value x : range(ms.at("form")))
    {
      if (ispair(x))
      {
        const value uid = m_gensym();
        copy_location(x, uid);
        binds = append(binds, list(list(uid, (*this)(x))));
        result = append(result, list(uid));
      }
      else
        result = append(result, list(x));
    }

    // Don't bloat output with empty let-statements
    if (binds == nil)
    {
      copy_location(fm, result);
      return result;
    }
    else
      return list("let", binds, result);
  });

  // atoms
  append_rule({nil, "x"}, [](const auto &ms) { return ms.at("x"); });
}

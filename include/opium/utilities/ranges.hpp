#pragma once

#include <ranges>


namespace opi::utl {


template <std::ranges::range Left, std::ranges::range Right>
struct zip_view: std::ranges::view_interface<zip_view<Left, Right>> {
  Left m_left;
  Right m_right;

  using left_iterator = std::ranges::iterator_t<Left>;
  using right_iterator = std::ranges::iterator_t<Right>;

  using left_value_type = std::ranges::range_value_t<Left>;
  using right_value_type = std::ranges::range_value_t<Right>;


  template <typename LeftIter, typename RightIter>
  struct iterator {
    using difference_type = ptrdiff_t;
    using value_type = std::pair<left_value_type, right_value_type>;

    iterator() = default;

    iterator(LeftIter left, RightIter right)
    : m_left_iter {left}, m_right_iter {right}
    { }

    iterator(const iterator &other)
    : m_left_iter {other.m_left_iter}, m_right_iter {other.m_right_iter}
    { }

    value_type
    operator * () const noexcept
    { return std::make_pair(*m_left_iter, *m_right_iter); }

    iterator&
    operator ++ () noexcept
    {
      std::advance(m_left_iter, 1);
      std::advance(m_right_iter, 1);
      return *this;
    }

    iterator
    operator ++ (int) const noexcept
    {
      iterator old = *this;
      ++*this;
      return old;
    }

    template <std::sentinel_for<LeftIter> LeftSentinel,
              std::sentinel_for<RightIter> RightSentinel>
    bool
    operator == (const iterator<LeftSentinel, RightSentinel> &other)
      const noexcept
    {
      return m_left_iter == other.m_left_iter and
             m_right_iter == other.m_right_iter;
    }

    LeftIter m_left_iter;
    RightIter m_right_iter;
  }; // struct opi::utl::zip_view::iterator
  static_assert(std::input_iterator<iterator<left_iterator, right_iterator>>);

  zip_view(const Left &left, const Right &right)
  : m_left {left}, m_right {right}
  { }

  zip_view(const zip_view &other)
  : m_left {other.m_left}, m_right {other.m_right}
  { }

  auto
  begin() const noexcept
  { return iterator {m_left.begin(), m_right.begin()}; }

  auto
  end() const noexcept
  { return iterator {m_left.end(), m_right.end()}; }
};


template <std::ranges::range Left, std::ranges::range Right>
auto
zip(Left &&left, Right &&right)
{
  return zip_view<std::views::all_t<Left>, std::views::all_t<Right>> {
      std::views::all(std::forward<Left>(left)),
      std::views::all(std::forward<Right>(right))};
}

} // namespace opi::utl

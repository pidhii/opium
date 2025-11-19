#include <eco/eco.h>

#include <type_traits>
#include <exception>
#include <optional>


namespace opi::utl {

struct guarded_stack {
  guarded_stack(size_t pages)
  : m_stack_owner {true}
  { eco_allocate_guarded_stack(pages, &m_eco_stack); }

  guarded_stack(const guarded_stack &) = delete;

  guarded_stack(guarded_stack&& other)
  : m_eco_stack {other.m_eco_stack}
  { other.m_stack_owner = false; }

  void*
  stack() const noexcept
  { return m_eco_stack.stack; }

  size_t
  size() const noexcept
  { return m_eco_stack.stack_size; }

  /** Can be used to test for running out of stack. */
  void*
  stack_high_end() const noexcept
  { return m_eco_stack.stack; }

  ~guarded_stack()
  { if (m_stack_owner) eco_destroy_guarded_stack(&m_eco_stack); }

  bool m_stack_owner;
  eco_stack_t m_eco_stack;
};


struct separate_stack_executor {
  separate_stack_executor(void *stack, size_t stack_size)
  : m_stack {stack},
    m_stack_size {stack_size}
  { }

  template <typename Func>
  struct entry_point_proxy {
    using result_type = std::invoke_result_t<Func>;

    entry_point_proxy(Func &func) : func {func}, exn {nullptr} {}

    static void
    entry_point(eco_t *, eco_t *, void *udata)
    {
      entry_point_proxy *self = static_cast<entry_point_proxy *>(udata);
      try
      {
        if constexpr (std::is_void_v<result_type>)
          self->func();
        else
          self->result = self->func();
      }
      catch (...)
      {
        self->exn = std::current_exception();
      }
    }

    Func &func;
    std::exception_ptr exn;
    std::conditional_t<std::is_void_v<result_type>, int,
                       std::optional<result_type>>
        result;
  };


  template <typename Func>
  auto
  operator () (Func &&func)
  {
    using proxy_type = entry_point_proxy<std::decay_t<Func>>;

    std::decay_t<Func> funccopy = std::forward<Func>(func);
    proxy_type proxy {funccopy};

    eco_t thisco, co;
    eco_init(&co, &proxy_type::entry_point, &thisco, m_stack,
             m_stack_size);
    eco_switch(&thisco, &co, (void **)&proxy, nullptr, nullptr);
    eco_cleanup(&co);

    if (proxy.exn)
      std::rethrow_exception(proxy.exn);

    if constexpr (not std::is_void_v<typename proxy_type::result_type>)
      return proxy.result.value();
  }

  private:
  void *m_stack;
  size_t m_stack_size;
};

} // namespace opi::utl
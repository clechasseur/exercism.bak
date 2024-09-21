#if !defined(CIRCULAR_BUFFER_DETAIL_H)
#define CIRCULAR_BUFFER_DETAIL_H

#include <functional>
#include <type_traits>
#include <utility>

namespace circular_buffer::detail {

// Type trait that detects if a parameter pack has a single type U
// and that type T is no-throw assignable from that type U.
template<typename, typename...>
struct is_nothrow_assignable_from_single : std::false_type {};
template<typename T, typename U>
struct is_nothrow_assignable_from_single<T, U> : std::is_nothrow_assignable<T, U> {};

// Helper variable template for the trait above.
template<typename T, typename... Ts>
inline constexpr bool is_nothrow_assignable_from_single_v = is_nothrow_assignable_from_single<T, Ts...>::value;

// Function that returns the first value in a parameter pack.
template<typename T, typename... Ts>
auto first_in_pack(T&& t, Ts&&...) -> T&& {
    return std::forward<T>(t);
}

// Simple class that executes a function when it is destroyed.
// Because function is called on stack unwinding, it should not throw.
class at_end_t final
{
public:
    template<typename F>
    explicit at_end_t(F&& f)
        : f_{std::forward<F>(f)} {}

    at_end_t(const at_end_t&) = delete;
    at_end_t(at_end_t&&) = delete;
    at_end_t& operator=(const at_end_t&) = delete;
    at_end_t& operator=(at_end_t&&) = delete;

    ~at_end_t() {
        f_();
    }

private:
    std::function<void()> f_;
};

// Helper to create an object that executes a function when destroyed.
// Meant to execute function on stack unwinding when a function exits.
template<typename F>
auto at_end(F&& f) -> at_end_t {
    return at_end_t{std::forward<F>(f)};
}

} // namespace circular_buffer::detail

#endif // CIRCULAR_BUFFER_DETAIL_H

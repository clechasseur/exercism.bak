#if !defined(CIRCULAR_BUFFER_DETAIL_H)
#define CIRCULAR_BUFFER_DETAIL_H

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

// Simple class that defers a function execution to the end of a scope.
// Name inspired by Go's `defer`. Implementation inspired by GSL's `final_act`:
// https://github.com/microsoft/GSL/blob/main/include/gsl/util#L70-L97
// Thanks to siebenschlaefer for pointing me in that direction!
template<typename F>
class defer final
{
public:
    explicit defer(const F& f) : f_{f} {}
    explicit defer(F&& f) : f_{std::move(f)} {}

    // GSL's `final_act` has an interesting move constructor that transfers
    // the responsibility of invoking the function to another instance.
    // We don't need it in our case however so I didn't copy it.
    defer(const defer&) = delete;
    defer(defer&&) = delete;
    defer& operator=(const defer&) = delete;
    defer& operator=(defer&&) = delete;

    ~defer() {
        f_();
    }

private:
    F f_;
};

} // namespace circular_buffer::detail

#endif // CIRCULAR_BUFFER_DETAIL_H

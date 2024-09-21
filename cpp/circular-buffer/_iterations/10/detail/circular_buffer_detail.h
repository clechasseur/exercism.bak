#if !defined(CIRCULAR_BUFFER_DETAIL_H)
#define CIRCULAR_BUFFER_DETAIL_H

#include <type_traits>
#include <utility>

namespace circular_buffer::detail {

// Type trait that detects if a parameter pack has a single type U
// and that type T is assignable from that type U.
template<typename, typename...>
struct is_assignable_from_single : std::false_type {};
template<typename T, typename U>
struct is_assignable_from_single<T, U> : std::is_assignable<T, U> {};

// Helper variable template for the trait above.
template<typename T, typename... Ts>
inline constexpr bool is_assignable_from_single_v = is_assignable_from_single<T, Ts...>::value;

// Function that returns the first value in a parameter pack.
template<typename T, typename... Ts>
auto first_in_pack(T&& t, Ts&&...) -> T&& {
    return std::forward<T>(t);
}

} // namespace circular_buffer::detail

#endif // CIRCULAR_BUFFER_DETAIL_H

#if !defined(CIRCULAR_BUFFER_H)
#define CIRCULAR_BUFFER_H

#include <memory>
#include <stdexcept>
#include <type_traits>

namespace circular_buffer {

namespace detail {

// Type trait that detects if a parameter pack has a single type U
// and that type T is assignable from that type U.
template<typename, typename... Ts>
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

} // namespace detail

template<typename T, typename Alloc = std::allocator<T>>
class circular_buffer final : private Alloc
{
    // We inherit from Alloc instead of aggregating it in order to trigger
    // EBO (Empty Base Optimization), see: https://en.cppreference.com/w/cpp/language/ebo
    // Thanks to ivan-shrimp for reminding me of this trick!

    using allocator_traits = std::allocator_traits<Alloc>;
    using pointer = typename allocator_traits::pointer;

public:
    using value_type = typename allocator_traits::value_type;
    using size_type = typename allocator_traits::size_type;

    explicit circular_buffer(size_type capacity)
        : capacity_{validate_capacity(capacity)},
          size_{0},
          pdata_{allocator_traits::allocate(*this, capacity_)},
          pcur_{pdata_} {}

    // Copy/move semantics are not required for this exercise so we'll just prohibit them.
    circular_buffer(const circular_buffer&) = delete;
    circular_buffer(circular_buffer&&) = delete;
    circular_buffer& operator=(const circular_buffer&) = delete;
    circular_buffer& operator=(circular_buffer&&) = delete;

    ~circular_buffer() {
        clear();
        allocator_traits::deallocate(*this, pdata_, capacity_);
    }

    auto size() const noexcept -> size_type {
        return size_;
    }

    auto is_empty() const noexcept -> bool {
        return size() == 0;
    }

    auto read() -> value_type {
        if (is_empty()) {
            throw_domain_error("cannot read() from an empty buffer");
        }
        
        auto value{std::move(*pcur_)};
        allocator_traits::destroy(*this, pcur_);
        pcur_ = wrap(pcur_ + 1);
        --size_;
        return value;
    }

    
    template<typename... Args>
    auto write(Args&&... args) {
        if (is_full()) {
            throw_domain_error("cannot write() in a full buffer");
        }

        overwrite(std::forward<Args>(args)...);
    }

    template<typename... Args>
    auto overwrite(Args&&... args) {
        auto write_p = get_write_p();

        if (is_full()) {
            if constexpr (detail::is_assignable_from_single_v<value_type, Args...>) {
                // We only have a single value in the pack and T is assignable from it,
                // so we can use assignment operator (possibly moving the value).
                *write_p = detail::first_in_pack(std::forward<Args>(args)...); // TODO do we need to use std::move here?
            } else if constexpr (std::is_constructible_v<value_type, Args...> && std::is_assignable_v<value_type&, value_type>) {
                // Either caller passed multiple values, or a value we can't assign to T,
                // but we can construct a T from them and assign it.
                *write_p = value_type{std::forward<Args>(args)...};
            } else {
                // We can't use assignment, so we have no choice but to destroy the old value and create a new one.
                static_assert(std::is_constructible_v<value_type, Args...>,
                              "writing to buffer requires type T to be constructible or assignable from argument");

                allocator_traits::destroy(*this, write_p);
                allocator_traits::construct(*this, write_p, std::forward<Args>(args)...);
            }

            pcur_ = wrap(pcur_ + 1);
        } else {
            allocator_traits::construct(*this, write_p, std::forward<Args>(args)...);
            ++size_;
        }
    }

    auto clear() {
        for (; size_ != 0; --size_, pcur_ = wrap(pcur_ + 1)) {
            allocator_traits::destroy(*this, pcur_);
        }
    }

private:
    size_type capacity_;
    size_type size_;
    pointer pdata_;
    pointer pcur_;

    auto is_full() const noexcept -> bool {
        return size() == capacity_;
    }

    auto wrap(pointer p) const noexcept -> pointer {
        return p >= (pdata_ + capacity_) ? (p - capacity_) : p;
    }

    auto get_write_p() const noexcept -> pointer {
        return wrap(pcur_ + size_);
    }

    static auto validate_capacity(size_type capacity) -> size_type {
        if (capacity == 0) {
            throw_domain_error("cannot create buffer with capacity 0");
        }

        return capacity;
    }

    [[noreturn]] static auto throw_domain_error(const char* what) {
        throw std::domain_error(what);
    }
};

}  // namespace circular_buffer

#endif // CIRCULAR_BUFFER_H

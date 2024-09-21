#if !defined(CIRCULAR_BUFFER_H)
#define CIRCULAR_BUFFER_H

#include "detail/circular_buffer_detail.h"

#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace circular_buffer {

template<typename T, typename Alloc = std::allocator<T>>
class circular_buffer final : private Alloc
{
    // We inherit from Alloc instead of aggregating it in order to trigger
    // EBO (Empty Base Optimization), see: https://en.cppreference.com/w/cpp/language/ebo
    // Thanks to ivan-shrimp for reminding me of this trick!

    using allocator_traits = std::allocator_traits<Alloc>;
    using pointer = typename allocator_traits::pointer;
    using difference_type = typename allocator_traits::difference_type;

public:
    using value_type = typename allocator_traits::value_type;
    using size_type = typename allocator_traits::size_type;

    explicit circular_buffer(size_type capacity)
        : capacity_{validate_capacity(capacity)},
          pdata_{allocator_traits::allocate(*this, capacity_)} {}

    // Copy/move semantics are not required for this exercise so we'll just prohibit them.
    circular_buffer(const circular_buffer&) = delete;
    circular_buffer(circular_buffer&&) = delete;
    circular_buffer& operator=(const circular_buffer&) = delete;
    circular_buffer& operator=(circular_buffer&&) = delete;

    ~circular_buffer() {
        clear();
        allocator_traits::deallocate(*this, pdata_, capacity_);
    }

    [[nodiscard]] auto size() const noexcept -> size_type {
        return size_;
    }

    [[nodiscard]] auto is_empty() const noexcept -> bool {
        return size() == 0;
    }

    auto read() -> value_type {
        if (is_empty()) {
            throw std::domain_error("cannot read() from an empty buffer");
        }

        // In order to safeguard against a potential exception being thrown
        // when moving/copying the data out of the buffer, we'll increment
        // the current index and reduce size _before_ we actually return the data.
        auto read_p = pdata_ + icur_;
        inc_cur();
        --size_;

        // Now return the value, calling destructor before function returns.
        auto destroy_at_end = detail::at_end([=] {
            allocator_traits::destroy(*this, read_p);
        });
        return std::move(*read_p);
    }
    
    template<typename... Args>
    auto write(Args&&... args) -> void {
        if (is_full()) {
            throw std::domain_error("cannot write() in a full buffer");
        }

        overwrite(std::forward<Args>(args)...);
    }

    template<typename... Args>
    auto overwrite(Args&&... args) -> void {
        auto write_p = get_write_p();

        if (is_full()) {
            if constexpr (detail::is_nothrow_assignable_from_single_v<value_type, Args...>) {
                // We only have a single value in the pack and T is no-throw assignable from it,
                // so we can use assignment operator (possibly moving the value).
                *write_p = detail::first_in_pack(std::forward<Args>(args)...);
                inc_cur();
            } else if constexpr (std::is_constructible_v<value_type, Args...> && std::is_nothrow_assignable_v<value_type&, value_type>) {
                // Either caller passed multiple values, or a value we can't assign to T,
                // but we can construct a T from them and no-throw assign it.
                *write_p = value_type{std::forward<Args>(args)...};
                inc_cur();
            } else {
                // We can't use no-throw assignment, so we have no choice but to destroy the old value and create a new one.
                static_assert(std::is_constructible_v<value_type, Args...>,
                              "writing to buffer requires type T to be constructible or assignable from argument");

                // To protect against an exception thrown when constructing the new value,
                // we'll destroy the old value, increment current pointer and reduce size,
                // _then_ we'll construct the new value and re-increment the size (if it worked).
                allocator_traits::destroy(*this, write_p);
                inc_cur();
                --size_;
                
                allocator_traits::construct(*this, write_p, std::forward<Args>(args)...);
                ++size_;
            }
        } else {
            allocator_traits::construct(*this, write_p, std::forward<Args>(args)...);
            ++size_;
        }
    }

    auto clear() -> void {
        for (; size_ != 0; inc_cur(), --size_) {
            allocator_traits::destroy(*this, pdata_ + icur_);
        }
    }

private:
    size_type capacity_;
    size_type size_{0};
    pointer pdata_;
    difference_type icur_{0};

    [[nodiscard]] auto is_full() const noexcept -> bool {
        return size() == capacity_;
    }

    auto inc_cur() noexcept -> void {
        icur_ = (icur_ + 1) % capacity_;
    }

    [[nodiscard]] auto get_write_p() const noexcept -> pointer {
        return pdata_ + ((icur_ + static_cast<difference_type>(size_)) % capacity_);
    }

    [[nodiscard]] static auto validate_capacity(size_type capacity) -> size_type {
        if (capacity == 0) {
            throw std::domain_error("cannot create buffer with capacity 0");
        }

        return capacity;
    }
};

}  // namespace circular_buffer

#endif // CIRCULAR_BUFFER_H

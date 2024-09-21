#include "circular_buffer.h"
#ifdef EXERCISM_TEST_SUITE
#include <catch2/catch.hpp>
#else
#include "test/catch.hpp"
#endif

#include <stdexcept>

TEST_CASE("test_size_and_is_empty")
{
    // We've added size() and is_empty() methods to the buffer class.

    circular_buffer::circular_buffer<int> buffer(3);

    REQUIRE(0 == buffer.size());
    REQUIRE(buffer.is_empty());

    REQUIRE_NOTHROW(buffer.write(42));
    REQUIRE(1 == buffer.size());
    REQUIRE(!buffer.is_empty());

    REQUIRE_NOTHROW(buffer.write(23));
    REQUIRE(2 == buffer.size());
    REQUIRE(!buffer.is_empty());

    REQUIRE(42 == buffer.read());
    REQUIRE(1 == buffer.size());
    REQUIRE(!buffer.is_empty());

    REQUIRE_NOTHROW(buffer.write(11));
    REQUIRE_NOTHROW(buffer.clear());
    REQUIRE(0 == buffer.size());
    REQUIRE(buffer.is_empty());
}

TEST_CASE("test_with_move_only_type") 
{
    // Buffer class should work for types that can be moved but not copied.

    struct foo {
        int i_;
        explicit foo(int i) : i_{i} {}
        foo(const foo&) = delete;
        foo(foo&&) = default;
        foo& operator=(const foo&) = delete;
        foo& operator=(foo&&) = default;
        operator int() const { return i_; }
    };
    circular_buffer::circular_buffer<foo> buffer(1);

    REQUIRE_NOTHROW(buffer.write(42));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(42));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(42));
    REQUIRE_NOTHROW(buffer.overwrite(23));
    REQUIRE(23 == buffer.read());

    REQUIRE_NOTHROW(buffer.write(foo{42}));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(foo{42}));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(foo{42}));
    REQUIRE_NOTHROW(buffer.overwrite(foo{23}));
    REQUIRE(23 == buffer.read());

    REQUIRE_NOTHROW(buffer.write(foo{42}));
    REQUIRE_NOTHROW(buffer.clear());
    REQUIRE(buffer.is_empty());
}

TEST_CASE("test_with_non_assignable_type")
{
    // Buffer class should work for types that can be constructed but not assigned.

    struct foo {
        int i_;
        explicit foo(int i) : i_{i} {}
        foo(const foo&) = default;
        foo(foo&&) = default;
        foo& operator=(const foo&) = delete;
        foo& operator=(foo&&) = delete;
        ~foo() = default;
        operator int() const { return i_; }
    };
    circular_buffer::circular_buffer<foo> buffer(1);

    REQUIRE_NOTHROW(buffer.write(42));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(42));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(42));
    REQUIRE_NOTHROW(buffer.overwrite(23));
    REQUIRE(23 == buffer.read());

    REQUIRE_NOTHROW(buffer.write(foo{42}));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(foo{42}));
    REQUIRE(42 == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(foo{42}));
    REQUIRE_NOTHROW(buffer.overwrite(foo{23}));
    REQUIRE(23 == buffer.read());

    REQUIRE_NOTHROW(buffer.write(42));
    REQUIRE_NOTHROW(buffer.clear());
}

TEST_CASE("test_variadic_write")
{
    // The write() and overwrite() method actually accept variadic arguments
    // and will construct a new instance with those arguments in the buffer.

    struct foo {
        int i_, j_;
        foo(int i, int j) : i_{i}, j_{j} {}
        bool operator==(const foo& rhs) const {
            return i_ == rhs.i_ && j_ == rhs.j_;
        }
    };
    circular_buffer::circular_buffer<foo> buffer(1);

    REQUIRE_NOTHROW(buffer.write(42, 23));
    REQUIRE(foo{42, 23} == buffer.read());

    REQUIRE_NOTHROW(buffer.overwrite(42, 23));
    REQUIRE_NOTHROW(buffer.overwrite(66, 67));
    REQUIRE(foo{66, 67} == buffer.read());

    // This works even if the type does not support assignment.
    
    struct bar : foo {
        bar(int i, int j) : foo{i, j} {}
        bar(const bar&) = default;
        bar(bar&&) = default;
        bar& operator=(const bar&) = delete;
        bar& operator=(bar&&) = delete;
    };
    circular_buffer::circular_buffer<bar> buffer2(1);

    REQUIRE_NOTHROW(buffer2.write(42, 23));
    REQUIRE(bar{42, 23} == buffer2.read());

    REQUIRE_NOTHROW(buffer2.overwrite(42, 23));
    REQUIRE_NOTHROW(buffer2.overwrite(66, 67));
    REQUIRE(bar{66, 67} == buffer2.read());
}

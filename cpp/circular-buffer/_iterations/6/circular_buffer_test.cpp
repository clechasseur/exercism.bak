#include "circular_buffer.h"
#ifdef EXERCISM_TEST_SUITE
#include <catch2/catch.hpp>
#else
#include "test/catch.hpp"
#endif

#include <stdexcept>

// Circular-buffer exercise test case data version 1.2.0

TEST_CASE("reading_empty_buffer_should_fail") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_THROWS_AS(buffer.read(), std::domain_error);
}

#define EXERCISM_RUN_ALL_TESTS

#if defined(EXERCISM_RUN_ALL_TESTS)
TEST_CASE("can_read_an_item_just_written") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));

    int expected = 1;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("each_item_may_only_be_read_once") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));

    int expected = 1;
    REQUIRE(expected == buffer.read());

    REQUIRE_THROWS_AS(buffer.read(), std::domain_error);
}

TEST_CASE("items_are_read_in_the_order_they_are_written") 
{
    circular_buffer::circular_buffer<int> buffer(2);

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_NOTHROW(buffer.write(2));

    int expected = 1;
    REQUIRE(expected == buffer.read());

    expected = 2;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("full_buffer_cant_be_written") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_THROWS_AS(buffer.write(2), std::domain_error);
}

TEST_CASE("a_read_frees_up_capacity_for_another_write") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));

    int expected = 1;
    REQUIRE(expected == buffer.read());

    REQUIRE_NOTHROW(buffer.write(2));

    expected = 2;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("read_position_is_maintained_even_across_multiple_writes") 
{
    circular_buffer::circular_buffer<int> buffer(3);

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_NOTHROW(buffer.write(2));

    int expected = 1;
    REQUIRE(expected == buffer.read());

    REQUIRE_NOTHROW(buffer.write(3));

    expected = 2;
    REQUIRE(expected == buffer.read());

    expected = 3;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("items_cleared_out_of_buffer_cant_be_read") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));

    buffer.clear();

    REQUIRE_THROWS_AS(buffer.read(), std::domain_error);
}

TEST_CASE("clear_frees_up_capacity_for_another_write") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));

    buffer.clear();

    REQUIRE_NOTHROW(buffer.write(2));

    int expected = 2;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("clear_does_nothing_on_empty_buffer") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    buffer.clear();

    REQUIRE_NOTHROW(buffer.write(1));

    int expected = 1;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("overwrite_acts_like_write_on_non_full_buffer") 
{
    circular_buffer::circular_buffer<int> buffer(2);

    REQUIRE_NOTHROW(buffer.write(1));

    buffer.overwrite(2);

    int expected = 1;
    REQUIRE(expected == buffer.read());

    expected = 2;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("overwrite_replaces_the_oldest_item_on_full_buffer") 
{
    circular_buffer::circular_buffer<int> buffer(2);

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_NOTHROW(buffer.write(2));

    buffer.overwrite(3);

    int expected = 2;
    REQUIRE(expected == buffer.read());

    expected = 3;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("overwrite_replaces_the_oldest_item_remaining_in_buffer_following_a_read") 
{
    circular_buffer::circular_buffer<int> buffer(3);

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_NOTHROW(buffer.write(2));
    REQUIRE_NOTHROW(buffer.write(3));

    int expected = 1;
    REQUIRE(expected == buffer.read());

    REQUIRE_NOTHROW(buffer.write(4));

    buffer.overwrite(5);

    expected = 3;
    REQUIRE(expected == buffer.read());

    expected = 4;
    REQUIRE(expected == buffer.read());

    expected = 5;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("full_buffer_cant_be_written_after_overwrite") 
{
    circular_buffer::circular_buffer<int> buffer(1);

    REQUIRE_NOTHROW(buffer.write(1));
    buffer.overwrite(2);
    REQUIRE_THROWS_AS(buffer.write(3), std::domain_error);

    int expected = 2;
    REQUIRE(expected == buffer.read());
}

TEST_CASE("check_correctness_with_string_type") 
{
    circular_buffer::circular_buffer<std::string> buffer(3);

    REQUIRE_NOTHROW(buffer.write("hello"));
    REQUIRE_NOTHROW(buffer.write("world"));
    REQUIRE_NOTHROW(buffer.write("zombies"));

    std::string expected = "hello";
    REQUIRE(expected == buffer.read());

    REQUIRE_NOTHROW(buffer.write("pants"));

    buffer.overwrite("banana");

    expected = "zombies";
    REQUIRE(expected == buffer.read());

    expected = "pants";
    REQUIRE(expected == buffer.read());

    expected = "banana";
    REQUIRE(expected == buffer.read());
}

TEST_CASE("initial_clear_does_not_affect_wrapping_around")
{
    circular_buffer::circular_buffer<int> buffer(2);

    buffer.clear();

    REQUIRE_NOTHROW(buffer.write(1));
    REQUIRE_NOTHROW(buffer.write(2));

    buffer.overwrite(3);
    buffer.overwrite(4);

    int expected = 3;
    REQUIRE(expected == buffer.read());

    expected = 4;
    REQUIRE(expected == buffer.read());

    REQUIRE_THROWS_AS(buffer.read(), std::domain_error);
}
#endif  // !EXERCISM_RUN_ALL_TESTS

// ----------------------------------------
// From this point, custom tests were added
// ----------------------------------------

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
    // and will construct a new instance with those arguments in the buffer
    // (a bit like the standard containers' emplace() method).

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

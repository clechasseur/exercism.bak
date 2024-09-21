#include "luhn.h"

#include <cctype>
#include <cstddef>
#include <optional>
#include <utility>

namespace luhn {

namespace {

template<typename T, typename It, typename Op>
auto try_fold(It first, It last, T init, Op op) -> std::optional<T>
{
    for (; first != last; ++first) {
        auto maybe_acc = op(std::move(init), *first);
        if (!maybe_acc) {
            return {};
        }
        init = std::move(*maybe_acc);
    }

    return std::make_optional(init);
}
    
} // anonymous namespace

auto valid(std::string_view number) -> bool
{
    std::size_t digit_count{0};
    auto maybe_sum = try_fold(number.crbegin(), number.crend(), 0,
        [&](auto acc, unsigned char c) -> std::optional<decltype(acc)> {
            if (c == ' ') {
                return std::make_optional(acc);
            } else if (!std::isdigit(c)) {
                return {};
            }

            decltype(acc) digit = c - '0';
            if (++digit_count % 2 == 0) {
                digit *= 2;
                if (digit > 9) {
                    digit -= 9;
                }
            }
            return std::make_optional(acc + digit);
        });

    return maybe_sum.has_value() &&
           digit_count > 1 &&
           *maybe_sum % 10 == 0;
}

}  // namespace luhn

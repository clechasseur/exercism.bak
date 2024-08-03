#include "all_your_base.h"

#include <algorithm>
#include <stdexcept>
#include <string>

namespace all_your_base {

auto convert(unsigned int in_base,
             const std::vector<unsigned int>& in_digits,
             unsigned int out_base) -> std::vector<unsigned int>
{
    if (in_base < 2) {
        throw std::invalid_argument("in_base must be >= 2");
    }
    if (out_base < 2) {
        throw std::invalid_argument("out_base must be >= 2");
    }

    unsigned long long n = 0;
    for (const auto digit : in_digits) {
        if (digit >= in_base) {
            throw std::invalid_argument("digits must be valid for in_base, found " + std::to_string(digit));
        }
        n = n * in_base + digit;
    }

    std::vector<unsigned int> out_digits;
    while (n > 0) {
        out_digits.emplace_back(n % out_base);
        n /= out_base;
    }
    std::reverse(out_digits.begin(), out_digits.end());
    return out_digits;
}

}  // namespace all_your_base

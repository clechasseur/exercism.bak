#if !defined(ALL_YOUR_BASE_H)
#define ALL_YOUR_BASE_H

#include <vector>

namespace all_your_base {

auto convert(unsigned int in_base,
             const std::vector<unsigned int>& in_digits,
             unsigned int out_base) -> std::vector<unsigned int>;

}  // namespace all_your_base

#endif // ALL_YOUR_BASE_H
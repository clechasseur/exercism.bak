#include "reverse_string.h"

#include <algorithm>
#include <utility>

namespace reverse_string {

std::string reverse_string(std::string s)
{
    // Reversing the string in-place seems like the most efficient choice
    // if we already have a `string`, since it does not allocate new memory.
    // Normally we could use `std::reverse`, but here it would probably be cheating. 🙂

    const auto middle_it = s.begin() + (s.size() / 2);
    for (auto [it_start, it_end] = std::make_pair(s.begin(), s.rbegin()); it_start != middle_it; ++it_start, ++it_end) {
        std::iter_swap(it_start, it_end);
    }
    return s;
}

}  // namespace reverse_string

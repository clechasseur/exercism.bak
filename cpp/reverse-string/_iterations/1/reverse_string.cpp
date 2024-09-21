#include "reverse_string.h"

#include <algorithm>

namespace reverse_string {

std::string reverse_string(std::string s)
{
    // Reversing the string in-place seems like the most efficient choice
    // if we already have a `string`, since it does not allocate new memory.
    // Normally we could use `std::reverse`, but here it would probably be cheating. 🙂

    auto swap_end_idx = s.size() / 2;
    for (decltype(swap_end_idx) i = 0; i < swap_end_idx; ++i) {
        std::iter_swap(s.begin() + i, s.end() - (i + 1));
    }
    return s;
}

}  // namespace reverse_string

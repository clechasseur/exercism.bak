#if !defined(PARALLEL_LETTER_FREQUENCY_H)
#define PARALLEL_LETTER_FREQUENCY_H

#include <cstddef>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace parallel_letter_frequency {

using string_views = std::vector<std::string_view>;
using frequencies = std::unordered_map<char, std::size_t>;

auto frequency(const string_views& texts) -> frequencies;

}

#endif


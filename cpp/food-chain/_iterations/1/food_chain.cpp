#include "food_chain.h"

#include <array>
#include <sstream>
#include <string_view>

namespace food_chain {

namespace {

constexpr std::string_view first_line_prefix = "I know an old lady who swallowed a ";
constexpr std::array<std::string_view, 9> animals = {
    "", "fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"
};
constexpr std::string_view spider_sense = "wriggled and jiggled and tickled inside her";
constexpr std::array<std::string_view, 8> second_line_prefixes = {
    "",
    "",
    "",
    "How absurd to swallow a ",
    "Imagine that, to swallow a ",
    "What a hog, to swallow a ",
    "Just opened her throat and swallowed a ",
    "I don't know how she swallowed a ",
};
constexpr std::string_view recursive_line_prefix = "She swallowed the ";
constexpr std::string_view recursive_line_suffix = " to catch the ";
constexpr std::string_view recursive_part_end = "I don't know why she swallowed the fly. Perhaps she'll die.";
constexpr std::string_view final_line = "She's dead, of course!";

void add_second_line(std::ostringstream& oss, std::size_t n)
{
    if (n == 8) {
        oss << final_line << "\n";
    } else if (n == 2) {
        oss << "It " << spider_sense << ".\n";
    } else if (n != 1) {
        oss << second_line_prefixes[n] << animals[n] << "!\n";
    }
}

void add_recursive_part(std::ostringstream& oss, std::size_t n)
{
    if (n == 1) {
        oss << recursive_part_end << "\n";
    } else if (n != 8) {
        oss << recursive_line_prefix << animals[n]
            << recursive_line_suffix << animals[n - 1];
        if (n == 3) {
            oss << " that " << spider_sense;
        }
        oss << ".\n";

        add_recursive_part(oss, n - 1);
    }
}

void add_first_part(std::ostringstream& oss, std::size_t n)
{
    oss << first_line_prefix << animals[n] << ".\n";
}

void add_second_part(std::ostringstream& oss, std::size_t n)
{
    add_second_line(oss, n);
    add_recursive_part(oss, n);
}

void add_verse(std::ostringstream& oss, std::size_t n)
{
    add_first_part(oss, n);
    add_second_part(oss, n);
}
    
} // anonymous namespace

auto verse(std::size_t n) -> std::string
{
    std::ostringstream oss;
    add_verse(oss, n);

    return oss.str();
}
    
auto verses(std::size_t from, std::size_t to) -> std::string
{
    std::ostringstream oss;
    for (std::size_t n = from; n <= to; ++n) {
        add_verse(oss, n);
        if (n < 8) {
            oss << "\n";
        }
    }

    return oss.str();
}
    
auto sing() -> std::string
{
    return verses(1, 8);
}

}  // namespace food_chain

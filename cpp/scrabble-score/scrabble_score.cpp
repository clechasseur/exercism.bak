#include "scrabble_score.h"

#include <cctype>
#include <numeric>
#include <string>
#include <unordered_map>

namespace scrabble_score {

namespace {

auto create_letter_scores() {
    const std::unordered_map<unsigned long, std::string> SCORE_TO_LETTERS{
        { 1, "aeioulnrst" },
        { 2, "dg" },
        { 3, "bcmp" },
        { 4, "fhvwy" },
        { 5, "k" },
        { 8, "jx" },
        { 10, "qz" },
    };

    return std::accumulate(SCORE_TO_LETTERS.cbegin(),
                           SCORE_TO_LETTERS.cend(),
                           std::unordered_map<char, unsigned long>{},
                           [](auto&& letter_scores, auto&& score_and_letters) {
                               auto&& [score, letters] = score_and_letters;
                               return std::accumulate(letters.cbegin(),
                                                      letters.cend(),
                                                      letter_scores,
                                                      [=](auto&& letter_scores, char c) {
                                                          letter_scores[c] = score;
                                                          return letter_scores;
                                                      });
                           });
}

const std::unordered_map<char, unsigned long> LETTER_SCORES{create_letter_scores()};

auto to_lower(char c) -> char
{
    // We first need to cast `c` to unsigned char, because if it
    // doesn't fit in an unsigned char we get undefined behavior.
    // For more info, see https://en.cppreference.com/w/cpp/string/byte/tolower
    return static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
}
    
} // anonymous namespace
    
auto score(std::string_view word) -> unsigned long
{
    return std::accumulate(word.cbegin(), word.cend(), 0, [](unsigned long score, char c) {
        return score + LETTER_SCORES.at(to_lower(c));
    });
}

}  // namespace scrabble_score

#if !defined(SCRABBLE_SCORE_H)
#define SCRABBLE_SCORE_H

#include <string_view>

namespace scrabble_score {

auto score(std::string_view word) -> unsigned long;

}  // namespace scrabble_score

#endif // SCRABBLE_SCORE_H
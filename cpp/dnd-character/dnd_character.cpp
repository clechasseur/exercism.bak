#include "dnd_character.h"

#include <algorithm>
#include <random>

namespace dnd_character {

namespace {

class dice_roller
{
public:
    auto operator()() -> int {
        return distribution_(generator_);
    }
    
private:
    std::mt19937 generator_{std::random_device{}()};
    std::uniform_int_distribution<int> distribution_{1, 6};
} roller;
    
} // anonymous namespace

int ability()
{
    int score = 0;
    int lowest = 6;
    
    for (int i = 0; i < 4; ++i) {
        const int die = roller();
        lowest = std::min(lowest, die);
        score += die;
    }
    
    score -= lowest;
    return score;
}
    
int modifier(int score)
{
    return (score / 2) - 5;
}
    
}  // namespace dnd_character

#include "secret_handshake.h"

#include <algorithm>
#include <cstddef>

namespace secret_handshake {

namespace {

const std::vector<std::string> ACTIONS{
    "wink",
    "double blink",
    "close your eyes",
    "jump",
};
constexpr unsigned int ACTION_REVERSE = 1 << 4;
    
} // anonymous namespace

auto commands(unsigned int actions) -> std::vector<std::string>
{
    std::vector<std::string> handshake;

    for (std::size_t i = 0; i < ACTIONS.size(); ++i) {
        if ((actions & (1 << i)) != 0) {
            handshake.emplace_back(ACTIONS[i]);
        }
    }
    if ((actions & ACTION_REVERSE) != 0) {
        std::reverse(handshake.begin(), handshake.end());
    }

    return handshake;
}
    
}  // namespace secret_handshake

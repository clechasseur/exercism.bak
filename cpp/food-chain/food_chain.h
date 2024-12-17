#if !defined(FOOD_CHAIN_H)
#define FOOD_CHAIN_H

#include <cstddef>
#include <string>

namespace food_chain {

auto verse(std::size_t n) -> std::string;
auto verses(std::size_t from, std::size_t to) -> std::string;
auto sing() -> std::string;
    
}  // namespace food_chain

#endif // FOOD_CHAIN_H
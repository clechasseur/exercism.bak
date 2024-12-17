#include "eliuds_eggs.h"

#include <bitset>

namespace chicken_coop {

auto positions_to_quantity(unsigned long long n) -> std::size_t
{
    return std::bitset<sizeof(unsigned long long) * 8>{n}.count();
}

}  // namespace chicken_coop

#if !defined(LUHN_H)
#define LUHN_H

#include <string_view>

namespace luhn {

auto valid(std::string_view number) -> bool;

}  // namespace luhn

#endif // LUHN_H
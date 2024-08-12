#if !defined(SECRET_HANDSHAKE_H)
#define SECRET_HANDSHAKE_H

#include <string>
#include <vector>

namespace secret_handshake {

auto commands(unsigned int actions) -> std::vector<std::string>;

}  // namespace secret_handshake

#endif // SECRET_HANDSHAKE_H

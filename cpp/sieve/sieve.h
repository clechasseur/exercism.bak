#if !defined(SIEVE_H)
#define SIEVE_H

#include <vector>

namespace sieve {

auto primes(int limit) -> std::vector<int>;

}  // namespace sieve

#endif // SIEVE_H
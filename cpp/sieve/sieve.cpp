#include "sieve.h"

namespace sieve {

auto primes(int limit) -> std::vector<int>
{
    std::vector<int> result;
    
    if (limit >= 2) {
        std::vector<bool> marks;
        marks.resize(limit - 1);
        
        for (int i = 2; i <= limit; ++i) {
            if (!marks[i - 2]) {
                result.push_back(i);
                for (int j = i + i; j <= limit; j += i) {
                    marks[j - 2] = true;
                }
            }
        }
    }
    
    return result;
}

}  // namespace sieve

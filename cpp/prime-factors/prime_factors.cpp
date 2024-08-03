#include "prime_factors.h"

#include <iostream>

#include <iostream>
using namespace std;

namespace prime_factors {
    bool is_prime( int n ) {
        for (auto i = 2 ; i <= n/2 ; i++ )
            if (n % i == 0) return false;
        return true;
    }


    vector<int> primes(int n) {
        vector<int> res;

        for (auto i = 2; i <= n ; i++)
            if (is_prime(i))  {
                res.push_back(i);
            }
                

        return res;
    }

    vector<int> of(int n) {
        vector<int> res, t = primes(n);
        int div; 

        if (n < 2) return {};
        if (is_prime(n)) return {n};

        for (auto it = t.begin() ; (n > 1) && (it != t.end()) ; it++ )
            {
                div = *it;

                while ( n % div == 0)
                    {
                        res.push_back(div);
                        n /= div;
                    }
            }
       
        return res;
        
    }
};  // namespace prime_factors

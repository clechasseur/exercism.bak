pub fn primes_up_to(upper_bound: usize) -> Vec<usize> {
    let mut candidates = vec![true; upper_bound + 1];
    let mut primes = Vec::new();
    for candidate in 2..=upper_bound {
        if candidates[candidate] {
            primes.push(candidate);
            let mut non_prime = candidate * 2;
            while non_prime <= upper_bound {
                candidates[non_prime] = false;
                non_prime += candidate;
            }
        }
    }
    primes
}

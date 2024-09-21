pub fn nth(n: u32) -> u32 {
    Primes::with_capacity(n as usize).nth(n as usize).unwrap()
}

#[derive(Debug, Default)]
struct Primes {
    primes: Vec<u32>,
    next_candidate: u32,
}

impl Primes {
    pub fn with_capacity(n: usize) -> Self {
        Self { primes: Vec::with_capacity(n), next_candidate: 0 }
    }

    fn is_prime(&self, candidate: u32) -> bool {
        !self.primes.iter().any(|prime| candidate % prime == 0)
    }
}

impl Iterator for Primes {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.primes.is_empty() {
            self.primes.push(2);
            self.next_candidate = 3;
            Some(2)
        } else {
            let next_prime = (self.next_candidate..)
                .step_by(2)
                .find(|candidate| self.is_prime(*candidate))
                .unwrap();
            self.primes.push(next_prime);
            self.next_candidate = next_prime + 2;
            Some(next_prime)
        }
    }
}

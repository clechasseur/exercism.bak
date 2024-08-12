use std::iter::FusedIterator;

pub fn factors(n: u64) -> Vec<u64> {
    PrimeFactors::of(n).collect()
}

#[derive(Debug, Copy, Clone)]
struct PrimeFactors {
    n: u64,
    divisor: u64,
}

impl PrimeFactors {
    pub fn of(n: u64) -> Self {
        Self { n, divisor: 2 }
    }

    fn inc_divisor(&mut self) {
        self.divisor = match self.divisor {
            2 => 3,
            n => n + 2,
        }
    }
}

impl Iterator for PrimeFactors {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.n {
            1 => None,
            _ => {
                while self.n % self.divisor != 0 {
                    self.inc_divisor();
                }
                self.n /= self.divisor;
                Some(self.divisor)
            },
        }
    }
}

impl FusedIterator for PrimeFactors {}

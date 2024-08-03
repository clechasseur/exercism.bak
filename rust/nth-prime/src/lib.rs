// This solution was ~~stolen~~ inspired by the following by @screamingjungle in the Elixir track:
// https://exercism.org/tracks/elixir/exercises/nth-prime/solutions/1a49ea41201e46ccba38a068cf710f49

// On my machine, running the entire test suite, including the 10,000nth prime test, takes ~13ms.
// The code in my 3rd iteration, on the other hand, took ~700ms to run the same tests.

pub fn nth(n: u32) -> u32 {
    Primes::default().nth(n as usize).unwrap()
}

#[derive(Debug, Copy, Clone)]
struct Primes {
    next: u32,
}

impl Primes {
    fn is_prime(n: &u32) -> bool {
        Self::is_prime_k(*n, 3)
    }

    fn is_prime_k(n: u32, k: u32) -> bool {
        n < k * k || (n % k != 0 && Self::is_prime_k(n, k + 2))
    }
}

impl Default for Primes {
    fn default() -> Self {
        Self { next: 2 }
    }
}

impl Iterator for Primes {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let next_prime = self.next;
        self.next = match next_prime {
            2 => 3,
            n => ((n + 2)..).step_by(2).find(Self::is_prime).unwrap(),
        };
        Some(next_prime)
    }
}

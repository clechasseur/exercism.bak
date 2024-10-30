pub fn collatz(n: u64) -> Option<u64> {
    fn rec_collatz(n: u64, count: u64) -> u64 {
        match n {
            1 => count,
            n if n % 2 == 0 => rec_collatz(n / 2, count + 1),
            n => rec_collatz(3 * n + 1, count + 1),
        }
    }

    (n != 0).then(|| rec_collatz(n, 0))
}

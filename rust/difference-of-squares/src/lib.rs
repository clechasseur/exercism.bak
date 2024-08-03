pub fn square_of_sum(n: u32) -> u32 {
    (n * (n + 1) / 2).pow(2)
}

pub fn sum_of_squares(n: u32) -> u32 {
    n * (n + 1) * (2 * n + 1) / 6
}

pub fn difference_classic(n: u32) -> u32 {
    square_of_sum(n) - sum_of_squares(n)
}

pub fn difference_smart(n: u32) -> u32 {
    // Taken from https://math.stackexchange.com/a/4063534
    ((n as f64) / 12.0 * (n.pow(2) - 1) as f64 * (3 * n + 2) as f64).floor() as u32
}

pub fn difference(n: u32) -> u32 {
    // According to Criterion, the "smart" solution is actually 6 times slower :D
    // (Benchmarks included; run them with "cargo bench --all-features")
    difference_classic(n)
}

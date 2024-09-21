pub fn square(s: u32) -> u64 {
    match s {
        1..=64 => 1u64 << (s - 1),
        _ => panic!("Square must be between 1 and 64"),
    }
}

pub fn total() -> u64 {
    // The total number of grains is the number of grains that would be on square 65, minus one.
    ((1u128 << 64) - 1) as u64
    // This would also work: `-1i64 as u64`
}

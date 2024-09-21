/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    let code: String = code.chars().filter(|&c| c != ' ').collect();
    code.len() > 1 &&
        code.chars()
            .rev()
            .map(|c| c.to_digit(10))
            .enumerate()
            .map(|(i, digit_opt)| digit_opt.map(|digit| {
                if i % 2 != 0 { double(digit) } else { digit }
            }))
            .sum::<Option<u32>>().unwrap_or(1) % 10 == 0
}

fn double(digit: u32) -> u32 {
    match digit * 2 {
        d if d > 9 => d - 9,
        d => d,
    }
}

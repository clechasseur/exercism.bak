/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    code.chars()
        .rev()
        .filter(|&c| !c.is_whitespace())
        .map(|c| c.to_digit(10))
        .enumerate()
        .map(|(i, digit_opt)| digit_opt.map(|digit| double_every_2nd(digit, i)))
        .try_fold((0, 0), |(sum, count), digit_opt| digit_opt.map(|digit| (sum + digit, count + 1)))
        .map_or(false, |(sum, count)| sum % 10 == 0 && count > 1)
}

#[inline(always)]
fn double(digit: u32) -> u32 {
    match digit * 2 {
        d if d > 9 => d - 9,
        d => d,
    }
}

#[inline(always)]
fn double_every_2nd(digit: u32, index: usize) -> u32 {
    if index % 2 != 0 {
        double(digit)
    } else {
        digit
    }
}

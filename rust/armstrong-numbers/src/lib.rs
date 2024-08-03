pub fn is_armstrong_number(num: u64) -> bool {
    let digits: Vec<_> = num.to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u64)
        .collect();
    let num_digits = digits.len() as u32;
    digits.iter().fold(0, |t, d| t + d.pow(num_digits)) == num
}

use std::fmt::Debug;

pub fn is_leap_year<T>(year: T) -> bool
where
    T: TryInto<i128>, // 128 bits should be enough for everybody
    <T as TryInto<i128>>::Error: Debug,
{
    let year = year.try_into().unwrap();
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_integer_types() {
        assert!(is_leap_year(120i8));
        assert!(is_leap_year(120u8));
        assert!(is_leap_year(120i16));
        assert!(is_leap_year(120u16));
        assert!(is_leap_year(120i32));
        assert!(is_leap_year(120u32));
        assert!(is_leap_year(120i64));
        assert!(is_leap_year(120u64));
        assert!(is_leap_year(120i128));
        assert!(is_leap_year(120u128));
    }
}

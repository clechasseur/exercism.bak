use core::fmt;
use core::fmt::Write;

// Length of the song's longest verse, including two separators.
// Will be used to pre-reserve space in the output string.
const LONGEST_VERSE: u32 = 128 + 2;

pub fn verse(n: u32) -> String {
    // Note: it may seem easier to implement the logic in `verse` and then simply
    // use some iterator in `sing` to `join` the resulting verses, but this will
    // cause multiple string allocations. Implementing `verse` in terms of `sing`
    // allows us to allocate memory only once.
    sing(n, n)
}

pub fn sing(start: u32, end: u32) -> String {
    try_sing(start, end).expect("String formatting failure")
}

fn try_sing(start: u32, end: u32) -> Result<String, fmt::Error> {
    // In theory, we could calculate the _exact_ amount of memory required for the verses asked,
    // but it would be a bit complex and I don't think a little over-allocation will kill us here.
    let mut output = String::with_capacity((LONGEST_VERSE * (start - end + 1)) as usize);

    write_verse(&mut output, start)?;
    if start > 0 {
        for bottles in (end..=(start - 1)).rev() {
            writeln!(&mut output)?;
            write_verse(&mut output, bottles)?;
        }
    }

    Ok(output)
}

fn write_verse(output: &mut String, bottles: u32) -> Result<(), fmt::Error> {
    write_n_bottles(output, bottles, true)?;
    write!(output, " of beer on the wall, ")?;
    write_n_bottles(output, bottles, false)?;
    write!(output, " of beer.\n{}, ", action(bottles))?;
    write_n_bottles(output, (bottles + 100 - 1) % 100, false)?;
    writeln!(output, " of beer on the wall.")
}

fn write_n_bottles(output: &mut String, bottles: u32, uppercase: bool) -> Result<(), fmt::Error> {
    match bottles {
        0 if uppercase => write!(output, "No more bottles"),
        0 => write!(output, "no more bottles"),
        1 => write!(output, "1 bottle"),
        n => write!(output, "{} bottles", n),
    }
}

fn action(bottles: u32) -> &'static str {
    match bottles {
        0 => "Go to the store and buy some more",
        1 => "Take it down and pass it around",
        _ => "Take one down and pass it around",
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "count-allocations")]
    #[test]
    fn test_allocations() {
        // This test validates that either "sing" or "verse" allocates memory only once.
        //
        // It requires the `allocation-counter` crate. To be able to run it, first add this to your `Cargo.toml`:
        //
        //   [features]
        //   count-allocations = ["allocation-counter"]
        //
        //   [dependencies]
        //   allocation-counter = { version = "0", optional = true }
        //
        // Then, run the test with:
        //
        //   cargo test --features count-allocations
        //
        // (I cannot submit the modified `Cargo.toml` with the exercise, since that breaks the test runner
        // because it is running in --offline mode and cannot fetch the additional crate.)

        let info = allocation_counter::measure(|| {
            crate::sing(99, 0);
        });
        assert_eq!(1, info.count_max);

        for n in 0..=99 {
            let info = allocation_counter::measure(|| {
                crate::verse(n);
            });
            assert_eq!(1, info.count_max);
        }
    }
}

//! Lightweight [`Forth`] interpreter.

use std::io;
use std::io::{stdin, stdout, BufRead, Write};
use std::ops::Deref;

use forth::Forth;

/// A small program wrapping our [`Forth`] interpreter, allowing the user to manually submit
/// sequences of words for evaluation.
fn main() -> Result<(), io::Error> {
    println!("Welcome to the Interactive Forth interpreter! Enter sequences of words to have them interpreted.");
    println!("Enter \"clear\" to reset to interpreter, or \"exit\" to exit.");

    let mut forth = Forth::new();
    loop {
        print!("> ");
        stdout().flush()?;
        let mut sequence = String::new();
        stdin().lock().read_line(&mut sequence)?;

        match sequence.deref().trim() {
            "clear" => forth = Forth::new(),
            "exit" => break,
            "" => (),
            sequence => forth
                .eval(sequence)
                .unwrap_or_else(|error| println!("ERROR: {:?}", error)),
        }

        println!("=> {:?}", forth.stack());
    }

    Ok(())
}

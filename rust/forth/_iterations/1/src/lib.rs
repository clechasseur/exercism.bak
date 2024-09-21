//! Implementation of an interpreter for a simple subset of Forth.
//!
//! Solution to an exercism on the Exercism Rust track: <https://exercism.org/tracks/rust/exercises/forth>
//!
//! # Documentation
//!
//! Because this is a more complex exercise, I decided to properly document the code using [rustdoc](https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html).
//! Because of the Markdown, it's sometimes a bit harder to read the doc in the code directly if you're not
//! used to the format. To generate the doc and open it in a browser, use
//!
//! ```sh
//! cargo doc --open
//! ```
//!
//! In order to ensure that all documentation is available when generated, everything in the code is `pub`lic; this would
//! not normally be the case of course.
//!
//! # Rustfmt
//!
//! The code has been formatted with the official Rust code formatter, [rustfmt](https://github.com/rust-lang/rustfmt).
//! The submission includes a `rustfmt.toml` file with the settings used for formatting. The settings depend on
//! some unstable features of rustfmt however, so in order to use it, you need a [Nightly Rust toolchain](https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust).
//! Then, to use rustfmt, use
//!
//! ```sh
//! cargo +nightly fmt
//! ```
//!
//! # Code coverage
//!
//! The `tarpaulin.toml` file is a configuration file for [tarpaulin](https://github.com/xd009642/tarpaulin), a Rust
//! code coverage tool. It's not necessary to install it to run the tests; I've used it to ensure proper coverage of
//! all test cases. If you do have tarpaulin [installed](https://github.com/xd009642/tarpaulin?tab=readme-ov-file#installation),
//! you can run it for the project using
//!
//! ```sh
//! cargo tarpaulin
//! ```

// These attributes make sure that everything in the code is properly documented.
// They are not necessary for running tests at all.
#![deny(missing_docs)]
#![deny(rustdoc::missing_crate_level_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod generation_map;
pub mod stack;
pub mod word;

use crate::stack::Stack;
use crate::word::builtins::add_builtin_words;
use crate::word::custom::CustomWord;
use crate::word::simple::SimpleWord;
use crate::word::{BoxedWord, Words};

/// Values that can be pushed to a Forth [`Stack`] (`i32`s).
pub type Value = i32;

/// [`Result`](std::result::Result) type used by this crate. Uses our [`Error`] type.
pub type Result<T> = std::result::Result<T, Error>;

/// A minimalistic Forth interpreter.
///
/// To use, first call [`eval`] to evaluate a series of "words". This will parse the sequence and execute the
/// corresponding operations, possibly manipulating the [`stack`]. Any definition of new (or even existing) words
/// will be saved in the interpreter for later.
///
/// It is possible to call [`eval`] multiple times; state is persisted between calls.
///
/// Then, the final result of the computation(s) can be checked by examining the interpreter's [`stack`].
///
/// # Examples
///
/// ```
/// use forth::Forth;
///
/// let mut forth = Forth::new();
/// assert!(forth.eval("1 2 + 3 * DUP 4 SWAP").is_ok());
/// assert!(forth
///     .eval(": deflabox OVER DUP ; deflabox 23 deflabox")
///     .is_ok());
/// assert_eq!(&[9, 4, 9, 4, 4, 23, 4, 4], forth.stack());
/// ```
///
/// [`eval`]: Forth::eval
/// [`stack`]: Forth::stack
pub struct Forth {
    stack: Stack,
    words: Words,
}

/// Types of errors that can occur when [`eval`]uating [`Forth`] sequences.
///
/// [`eval`]: Forth::eval
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    /// An attempt was made to divide a number by zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::{Error, Forth};
    ///
    /// let mut forth = Forth::new();
    /// assert_eq!(Err(Error::DivisionByZero), forth.eval("42 0 /"));
    /// ```
    DivisionByZero,

    /// An operation required a certain number of values to be on the stack, but the stack didn't have enough values.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::{Error, Forth};
    ///
    /// let mut forth = Forth::new();
    /// assert_eq!(Err(Error::StackUnderflow), forth.eval("1 DROP DROP"));
    /// ```
    StackUnderflow,

    /// An unknown word was specified.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::{Error, Forth};
    ///
    /// let mut forth = Forth::new();
    /// assert_eq!(
    ///     Err(Error::UnknownWord),
    ///     forth.eval("Cahf ah nafl mglw'nafh hh' ahor syha'h ah'legeth, ng llll or'azath syha'hnahh n'ghftephai n'gha ahornah ah'mglw'nafh"),
    /// );
    /// ```
    UnknownWord,

    /// The name specified for a custom word was invalid (e.g., it was a number).
    ///
    /// OR
    ///
    /// A custom word definition was not terminated properly.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::{Error, Forth};
    ///
    /// let mut forth = Forth::new();
    /// assert_eq!(Err(Error::InvalidWord), forth.eval(": 1 2 ;"));
    /// assert_eq!(Err(Error::InvalidWord), forth.eval(": unfinished"));
    /// ```
    InvalidWord,
}

impl Forth {
    /// Creates a new [`Forth`] interpreter with an empty [`stack`] and no known custom words.
    ///
    /// # Built-in words
    ///
    /// The interpreter will know all built-in words by default:
    ///
    /// | Built-in word | Effect                                                                      | Implementation                          |
    /// |---------------|-----------------------------------------------------------------------------|-----------------------------------------|
    /// | `+`           | Add the top two values on the stack, push result                            | [`plus`](word::builtins::arith::plus)   |
    /// | `-`           | Subtract the top stack value from the second-to-top one, push result        | [`minus`](word::builtins::arith::minus) |
    /// | `*`           | Multiply the top two values on the stack, push result                       | [`times`](word::builtins::arith::times) |
    /// | `/`           | Divide the second-to-top value on the stack by the top one, push result     | [`quot`](word::builtins::arith::quot)   |
    /// | `DUP`         | Duplicate the top stack value                                               | [`dup`](word::builtins::stack::dup)     |
    /// | `DROP`        | Drop the top stack value, discarding it                                     | [`drop`](word::builtins::stack::drop)   |
    /// | `SWAP`        | Swap the first two values on the stack                                      | [`swap`](word::builtins::stack::swap)   |
    /// | `OVER`        | Read the second-to-last stack value (without removing it), push it again    | [`over`](word::builtins::stack::over)   |
    ///
    /// [`stack`]: Forth::stack
    pub fn new() -> Self {
        let mut words = Words::new();
        add_builtin_words(&mut words);

        Self { stack: Stack::new(), words }
    }

    /// Returns a reference to the current values on the interpreter's stack.
    pub fn stack(&self) -> &[Value] {
        &self.stack
    }

    /// Evaluates an input sequence and executes its operations.
    ///
    /// After execution, it is possible to access the values on the [`stack`](Forth::stack) to see the final result(s).
    ///
    /// This method can be called multiple times. The stack and any defined custom words will persist between calls.
    ///
    /// # Defining words
    ///
    /// Words can be defined using the following syntax:
    ///
    /// ```forth
    /// : name instructions... ;
    /// ```
    ///
    /// This creates a word called `name` that, when executed, will result in the execution of its corresponding
    /// instructions. Note that _defining_ the word does not execute it per se; it needs to be passed later in the
    /// sequence. Example:
    ///
    /// ```forth
    /// : foo 2 dup ; foo
    /// ```
    ///
    /// will result in the following stack:
    ///
    /// ```forth
    /// 2 2
    /// ```
    ///
    /// Corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth.eval(": foo 2 dup ; foo").is_ok());
    /// assert_eq!(&[2, 2], forth.stack());
    /// ```
    ///
    /// # Redefinitions
    ///
    /// It is possible to define a certain word multiple times. When a word is redefined, any reference to the word
    /// _after that point_ in the sequence will use the new definition, but references to the word _before that point_
    /// will still refer to the previous definition. Example:
    ///
    /// ```forth
    /// : foo 1 ; : bar foo ; : foo 2 ; foo bar
    /// ```
    ///
    /// Here, `foo` is defined twice, but the reference to `foo` inside the definition of `bar` refers to the _first_
    /// definition of `foo`. Thus, the execution of `foo bar` will leave the stack with
    ///
    /// ```forth
    /// 2 1
    /// ```
    ///
    /// Corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth
    ///     .eval(": foo 1 ; : bar foo ; : foo 2 ; foo bar")
    ///     .is_ok());
    /// assert_eq!(&[2, 1], forth.stack());
    /// ```
    ///
    /// # Overriding built-in words
    ///
    /// It is possible to redefine a built-in word, even an arithmetic operator. For example:
    ///
    /// ```forth
    /// 1 2 + : + - ; 3 4 +
    /// ```
    ///
    /// will result in the following stack:
    ///
    /// ```forth
    /// 3 -1
    /// ```
    ///
    /// Corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth.eval("1 2 + : + - ; 3 4 +").is_ok());
    /// assert_eq!(&[3, -1], forth.stack());
    /// ```
    ///
    /// # Definitions within definitions
    ///
    /// It is possible to define a word within the definition to another word[^1]; however, all words are stored in the
    /// dictionary at the same level (e.g. the inner definition will not be scoped to the outer definition only). Example:
    ///
    /// ```forth
    /// : foo : bar 2 ; bar bar ; foo bar
    /// ```
    ///
    /// will rsult in the following stack:
    ///
    /// ```forth
    /// 2 2 2
    /// ```
    ///
    /// Corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth.eval(": foo : bar 2 ; bar bar ; foo bar").is_ok());
    /// assert_eq!(&[2, 2, 2], forth.stack());
    /// ```
    ///
    /// Note however that when words are added to the dictionary, they are bound to the current generation (see
    /// [`Words::insert`]) and that they are added to the dictionary when the terminal `;` token is encountered.
    /// Thus, an inner definition will be added with a lower generation that its outer definition. This can be used
    /// to implement seemingly-recursive words:
    ///
    /// ```forth
    /// : foo : foo 2 ; foo ; foo foo
    /// ```
    ///
    /// It seems like this shouldn't work, but when `foo` is encountered in the outer `foo` definition, it will refer
    /// to its inner definition, while the `foo`s outside of both definitions will refer to the outer definition. This
    /// will result in the following stack:
    ///
    /// ```forth
    /// 2 2
    /// ```
    ///
    /// Corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth.eval(": foo : foo 2 ; foo ; foo foo").is_ok());
    /// assert_eq!(&[2, 2], forth.stack());
    /// ```
    ///
    /// # Eager validation
    ///
    /// Existence of words referred to in the sequence is validated when the word is encountered. Thus, an unknown
    /// word will result in an [`Err(Error::UnknownWord)`](Error::UnknownWord), but the rest of the sequence will not
    /// be executed. For example:
    ///
    /// ```
    /// use forth::{Error, Forth};
    ///
    /// let mut forth = Forth::new();
    /// assert_eq!(Err(Error::UnknownWord), forth.eval("foo 1 2"));
    /// assert!(forth.stack().is_empty());
    /// ```
    ///
    /// # Case insensitivity
    ///
    /// Words are evaluated in a case-insensitive manner. For example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth.eval("1 DUP dup DuP dUp").is_ok());
    /// assert_eq!(&[1, 1, 1, 1, 1], forth.stack());
    /// ```
    ///
    /// # Errors
    ///
    /// See [`Error`] for details on possible error conditions.
    ///
    /// [^1]: Definitions within definitions is not actually validated by the exercise tests and there are probably
    ///       multiple ways of implementing it, so this support might be considered experimental.
    pub fn eval(&mut self, input: &str) -> Result<()> {
        self.parse(&mut input.split_whitespace(), true)
            .and_then(|word| word.call(&mut self.stack, &self.words))
    }

    /// Parses a sequence of tokens and interprets it as a series of Forth instructions, then returns a single
    /// [`BoxedWord`] that, when [`call`]ed, will execute the instructions in the sequence. Any word definitions
    /// encountered will add the words to the internal dictionary.
    ///
    /// Any unknown word encountered will result in an [`Err(Error::UnknownWord)`] being returned and the parsing will
    /// immediately stop. The remaining tokens will not be interpreted. (See [`eval`]'s documentation for details)
    ///
    /// To interpret a sequence of tokens stored in a string slice, use:
    ///
    /// ```ignore
    /// let input = "1 2 +";
    /// let entire_sequence_as_a_word = self.parse(&mut input.split_whitespace(), true); // Pass `true` when calling initially since it's the topmost call
    /// ```
    ///
    /// [`call`]: word::Word::call
    /// [`Err(Error::UnknownWord)`]: Error::UnknownWord
    /// [`eval`]: Forth::eval
    fn parse<'a, I>(&mut self, tokens: &mut I, topmost: bool) -> Result<BoxedWord>
    where
        I: Iterator<Item = &'a str>,
    {
        let mut inner_words = Vec::new();

        loop {
            match tokens.next() {
                Some(":") => self.parse_custom_word(tokens)?,
                Some(";") if topmost => return Err(Error::InvalidWord),
                Some(";") => break,
                Some(token) => inner_words.push(self.parse_simple_word(token)?),
                None if topmost => break,
                None => return Err(Error::InvalidWord),
            }
        }

        Ok(Box::new(CustomWord::new(inner_words)))
    }

    /// Parses a token in a sequence and interprets it as a [`SimpleWord`].
    ///
    /// If the token is a [`Word`], it is converted to uppercase to allow for case-insensitive word lookups.
    /// (see [`eval`]'s documentation for details)
    ///
    /// If the token is a word that is not currently defined in the dictionary, [`Err(Error::UnknownWord)`] is returned.
    ///
    /// [`Word`]: SimpleWord::Word
    /// [`Err(Error::UnknownWord)`]: Error::UnknownWord
    /// [`eval`]: Forth::eval
    fn parse_simple_word(&self, token: &str) -> Result<SimpleWord> {
        match token.parse::<Value>() {
            Ok(number) => Ok(SimpleWord::Number(number)),
            Err(_) => {
                let word = token.to_uppercase();

                // Validate that this word is already defined. This detects unknown words early.
                // It also prevents potentially infinitely-recursive custom words.
                match self.words.get(&word) {
                    Some(_) => Ok(SimpleWord::Word(self.words.generation(), word)),
                    None => Err(Error::UnknownWord),
                }
            },
        }
    }

    /// Given a token sequence, processes tokens as a new word definition. The first token is the word name itself
    /// and the other tokens are the inner sequence of words. Stops when encountering the terminal `;` token and leaves
    /// any remaining tokens in the iterator alone. The resulting definition is added to the interpreter's dictionary.
    ///
    /// All [`Word`]s found are converted to uppercase to allow for case-insensitive lookups. (see [`eval`]'s
    /// documentation for details)
    ///
    /// If the word name is invalid (e.g. is a number) or if no terminal `;` is found, [`Err(Error::InvalidWord)`]
    /// is returned.
    ///
    /// [`Word`]: SimpleWord::Word
    /// [`eval`]: Forth::eval
    /// [`Err(Error::InvalidWord)`]: Error::InvalidWord
    fn parse_custom_word<'a, I>(&mut self, tokens: &mut I) -> Result<()>
    where
        I: Iterator<Item = &'a str>,
    {
        let word = tokens.next().ok_or(Error::InvalidWord).and_then(|word| {
            match word.parse::<Value>() {
                Ok(_) => Err(Error::InvalidWord),
                Err(_) => Ok(word.to_uppercase()),
            }
        })?;
        let op = self.parse(tokens, false)?;
        self.words.insert(word, op);
        Ok(())
    }
}

impl Default for Forth {
    /// Creates a new, empty [`Forth`] interpreter.
    ///
    /// This is the same as calling [`Forth::new`].
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let forth = Forth::default();
        assert!(forth.stack().is_empty());
    }
}

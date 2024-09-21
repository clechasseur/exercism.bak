//! Definition of "simple words" - either numbers or references to an existing word.

use crate::generation_map::Generation;
use crate::stack::Stack;
use crate::word::{Word, Words};
use crate::Value;

/// A simple Forth [`Word`].
///
/// Basically, any word that is not a custom word (defined via `: name tokens... ;`) is a simple word. This includes
/// numbers and references to existing words.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleWord {
    /// A number, like `1`.
    ///
    /// When [called](Word::call), the number will be pushed to the stack.
    Number(Value),

    /// A reference to an existing word.
    ///
    /// When [called](Word::call), the corresponding word will be fetched from the [dictionary](Words), using
    /// a max generation set when this reference was created. The operation tied to the word found will then
    /// be executed.
    ///
    /// # Use of max generation
    ///
    /// When a [`SimpleWord::Word`] is created, the current [generation](crate::generation_map::GenerationMap::generation)
    /// of the [dictionary](Words) is saved along with it. This way, if the same word is later redefined, we won't "see"
    /// the new definition. For example, let's say we [`eval`](crate::Forth::eval) the given sequence:
    ///
    /// ```forth
    /// : deflabox dup ; 1 deflabox : deflabox drop ; 1 deflabox
    /// ```
    ///
    /// This sequence will first define a new word `deflabox` that is an alias for `DUP`. It then pushes `1` to the stack
    /// and calls `deflabox`, which will `DUP` the `1`, leaving the stack as
    ///
    /// ```forth
    /// 1 1
    /// ```
    ///
    /// Next, `deflabox` is redefined as an alias for `DROP`. Another `1` is then pushed to the stack, and then `deflabox`
    /// is called again, this time dropping the top `1`. The final stack will be
    ///
    /// ```forth
    /// 1 1
    /// ```
    ///
    /// When `deflabox` is first parsed, a [`SimpleWord::Word`] will be created to refer to it. Because it will contain
    /// the dictionary's current generation, when we later execute all instructions, it will not "see" the later redefinition
    /// of `deflabox` and will correctly perform a `DUP` (instead of a `DROP`, which is what would've happened had it
    /// not saved the correct max generation).
    ///
    /// The corresponding Rust example:
    ///
    /// ```
    /// use forth::Forth;
    ///
    /// let mut forth = Forth::new();
    /// assert!(forth
    ///     .eval(": deflabox dup ; 1 deflabox : deflabox drop ; 1 deflabox")
    ///     .is_ok());
    /// assert_eq!(&[1, 1], forth.stack());
    /// ```
    Word(Generation, String),
}

impl Word for SimpleWord {
    /// "Calls" the word. The effect depends on the value of [`self`](SimpleWord):
    ///
    /// | `self`            | Effect of `call`                                                                              |
    /// |-------------------|-----------------------------------------------------------------------------------------------|
    /// | `Number(n)`       | Pushes `n` to the stack                                                                       |
    /// | `Word(gen, word)` | Calls [`Words::get_maxed(&word, gen)`] on the dictionary and [`call`]s the corresponding word |
    ///
    /// [`Words::get_maxed(&word, gen)`]: Words::get_maxed
    /// [`call`]: Word::call
    fn call(&self, stack: &mut Stack, dictionary: &Words) -> crate::Result<()> {
        match self {
            Self::Number(v) => {
                stack.push(*v);
                Ok(())
            },
            Self::Word(generation, word) => {
                // Existence of words referred to was validated in `Forth::parse`, so we can `expect` here.
                dictionary
                    .get_maxed(word, *generation)
                    .expect("Words should have been validated beforehand")
                    .call(stack, dictionary)
            },
        }
    }
}

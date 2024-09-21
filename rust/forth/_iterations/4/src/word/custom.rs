//! Definition of custom (e.g. user-defined) words.

use crate::stack::Stack;
use crate::word::simple::SimpleWord;
use crate::word::{Word, Words};
use crate::Result;

/// A custom (e.g. user-defined) word.
///
/// A custom word is an alias for a sequence of words that are executed when the custom word is [`call`]ed.
///
/// [`call`]: Word::call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomWord {
    inner_words: Vec<SimpleWord>,
}

impl CustomWord {
    /// Creates a new [`CustomWord`] as an alias for a sequence of [`SimpleWord`]s.
    pub fn new(inner_words: Vec<SimpleWord>) -> Self {
        Self { inner_words }
    }
}

impl Word for CustomWord {
    /// "Calls" the custom word, executing its inner sequence of words.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ops::Deref;
    ///
    /// use forth::stack::Stack;
    /// use forth::word::builtins::add_builtin_words;
    /// use forth::word::custom::CustomWord;
    /// use forth::word::simple::SimpleWord;
    /// use forth::word::{Word, Words};
    ///
    /// let mut words = Words::new();
    /// add_builtin_words(&mut words);
    ///
    /// let inner_words = vec![
    ///     SimpleWord::Number(1),
    ///     SimpleWord::Number(2),
    ///     SimpleWord::Word(words.generation(), "+".into()),
    ///     SimpleWord::Word(words.generation(), "DUP".into()),
    /// ];
    /// let custom_word = CustomWord::new(inner_words);
    ///
    /// let mut stack = Stack::new();
    /// assert!(custom_word.call(&mut stack, &words).is_ok());
    /// assert_eq!(&[3, 3], stack.deref());
    /// ```
    fn call(&self, stack: &mut Stack, dictionary: &Words) -> Result<()> {
        self.inner_words
            .iter()
            .try_fold((), |_, word| word.call(stack, dictionary))
    }
}

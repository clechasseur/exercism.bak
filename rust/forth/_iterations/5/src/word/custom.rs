//! Definition of custom (e.g. user-defined) [`Word`]s.

use std::rc::Rc;

use crate::stack::Stack;
use crate::word::{Word, WordRc, Words};
use crate::Result;

/// A custom (e.g. user-defined) [`Word`].
///
/// A custom word is an alias for a sequence of [`Word`]s that are executed when the custom word is [`call`]ed.
///
/// [`call`]: CustomWord::call
pub struct CustomWord {
    inner_words: Vec<WordRc>,
}

impl CustomWord {
    /// Creates a new [`CustomWord`] as an alias for a sequence of existing [`Word`]s, then wraps it in a [`WordRc`].
    pub fn wrap(inner_words: Vec<WordRc>) -> WordRc {
        Rc::new(Self { inner_words })
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
    /// use forth::word::value::ValueWord;
    /// use forth::word::{Word, WordRc, Words};
    ///
    /// let mut words = Words::new();
    /// add_builtin_words(&mut words);
    ///
    /// let inner_words =
    ///     vec![ValueWord::wrap(1), ValueWord::wrap(2), words.get("+")?, words.get("DUP")?];
    /// let custom_word = CustomWord::wrap(inner_words);
    ///
    /// let mut stack = Stack::new();
    /// assert!(custom_word.call(&mut stack, &words).is_ok());
    /// assert_eq!(&[3, 3], stack.deref());
    /// # Ok::<(), forth::Error>(())
    /// ```
    fn call(&self, stack: &mut Stack, dictionary: &Words) -> Result<()> {
        self.inner_words
            .iter()
            .try_fold((), |_, word| word.call(stack, dictionary))
    }
}

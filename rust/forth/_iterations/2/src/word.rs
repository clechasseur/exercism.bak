//! Types used to describe words understood by the [`Forth`](crate::Forth) interpreter.

pub mod builtins;
pub mod custom;
pub mod simple;

use crate::generation_map::GenerationMap;
use crate::stack::Stack;
use crate::Result;

/// Implementation of a Forth word.
///
/// Acts a bit like a [`Fn`]: can be [`call`]ed to interact with the [`Forth`] interpreter's [`Stack`].
///
/// [`Forth`]: crate::Forth
/// [`call`]: Word::call
pub trait Word {
    /// "Calls" the word, instructing it to perform its operation on the Forth interpreter's stack.
    ///
    /// The word should use the stack to fetch any parameter required and any output from the operation
    /// should then be pushed to the stack.
    ///
    /// The provided dictionary can be used to fetch definitions for other words; it will be used
    /// to implement [custom words](custom::CustomWord).
    fn call(&self, stack: &mut Stack, dictionary: &Words) -> Result<()>;
}

/// A Forth [`Word`], stored in a [`Box`]. Used to store dynamic words in structures.
pub type BoxedWord = Box<dyn Word + 'static>;

/// A map of [`BoxedWord`]s, associated with a String representation of the word.
///
/// The words are stored in a [`GenerationMap`], so words can be defined multiple times and we can still
/// access versions from previous generations.
pub type Words = GenerationMap<String, BoxedWord>;

impl<F> Word for F
where
    F: Fn(&mut Stack, &Words) -> Result<()>,
{
    /// Implementation of [`Word`] for a function with the same signature as [`Word::call`].
    ///
    /// Will be used to define built-in words like `+`, `DUP`, etc.
    ///
    /// To create, use [`box_fn_op`].
    fn call(&self, stack: &mut Stack, dictionary: &Words) -> Result<()> {
        self(stack, dictionary)
    }
}

/// Given a function that has a signature similar to [`Word::call`], boxes it into a [`BoxedWord`].
///
/// # Examples
///
/// ```
/// use std::ops::Deref;
///
/// use forth::stack::Stack;
/// use forth::word::{box_fn_op, Words};
/// use forth::Result;
///
/// fn add_10(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
///     let value = stack.pop()?;
///     stack.push(value + 10);
///     Ok(())
/// }
/// let boxed_add_10 = box_fn_op(add_10);
///
/// let mut stack = Stack::new();
/// stack.push(2);
/// let dictionary = Words::new();
///
/// assert!(boxed_add_10.call(&mut stack, &dictionary).is_ok());
/// assert_eq!(&[12], stack.deref());
/// ```
pub fn box_fn_op<F>(f: F) -> BoxedWord
where
    F: Fn(&mut Stack, &Words) -> Result<()> + 'static,
{
    Box::new(f)
}

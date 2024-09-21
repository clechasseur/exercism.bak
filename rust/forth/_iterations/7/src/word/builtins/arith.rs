//! Definition of built-in arithmetic Forth words, like `+`.

use crate::stack::Stack;
use crate::word::Words;
use crate::{Error, Result};

// A little helper macro to save on code duplication for simple arithmetic operators.
macro_rules! arith_op {
    (
        $(#[$attr:meta])*
        $vis:vis $nam:ident($arith_op:tt);
    ) => {
        $(#[$attr])*
        $vis fn $nam(stack: &mut $crate::stack::Stack, _dictionary: &$crate::word::Words) -> $crate::Result<()> {
            let b = stack.pop()?;
            let a = stack.pop()?;
            stack.push(a $arith_op b);
            Ok(())
        }
    };
}

arith_op! {
    /// Implementation of the `+` Forth word.
    ///
    /// Pops two [`Value`]s from the stack, adds them together and pushes the result to the stack.
    ///
    /// # Errors
    ///
    /// - [`Error::StackUnderflow`] - the stack does not have two values.
    ///
    /// [`Value`]: crate::Value
    pub plus(+);
}
arith_op! {
    /// Implementation of the `-` Forth word.
    ///
    /// Pops two [`Value`]s from the stack, subtract them and pushes the result to the stack. (The
    /// first value popped will be subtracted from the second value popped.)
    ///
    /// # Errors
    ///
    /// - [`Error::StackUnderflow`] - the stack does not have two values.
    ///
    /// [`Value`]: crate::Value
    pub minus(-);
}
arith_op! {
    /// Implementation of the `*` Forth word.
    ///
    /// Pops two [`Value`]s from the stack, multiplies them together and pushes the result to the stack.
    ///
    /// # Errors
    ///
    /// - [`Error::StackUnderflow`] - the stack does not have two values.
    ///
    /// [`Value`]: crate::Value
    pub times(*);
}

/// Implementation of the `/` Forth word.
///
/// Pops the denominator then the numerator from the stack, divides the numerator by the denominator
/// and pushes the result to the stack.
///
/// # Errors
///
/// - [`Error::StackUnderflow`] - the stack does not have two values.
/// - [`Error::DivisionByZero`] - the denominator is 0.
///
/// [`Value`]: crate::Value
pub fn quot(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
    let den = stack.pop()?;
    let num = stack.pop()?;
    match den {
        0 => Err(Error::DivisionByZero),
        den => {
            stack.push(num / den);
            Ok(())
        },
    }
}

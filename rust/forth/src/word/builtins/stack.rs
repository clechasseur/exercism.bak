//! Definition of built-in Forth words related to stack manipulation, like `DUP`.

use crate::stack::Stack;
use crate::word::Words;
use crate::Result;

/// Duplicates the top stack value.
///
/// # Errors
///
/// - [`Error::StackUnderflow`] - the stack is empty.
///
/// [`Error::StackUnderflow`]: crate::Error::StackUnderflow
pub fn dup(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
    stack.push(stack.peek()?);
    Ok(())
}

/// Removes the top element of the stack, discarding it.
///
/// # Errors
///
/// - [`Error::StackUnderflow`] - the stack is empty.
///
/// [`Error::StackUnderflow`]: crate::Error::StackUnderflow
pub fn drop(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
    stack.pop().map(|_| ())
}

/// Removes the top two values from the stack and pushes them back again in reverse order.
///
/// # Errors
///
/// - [`Error::StackUnderflow`] - the stack did not have at least two values.
///
/// [`Error::StackUnderflow`]: crate::Error::StackUnderflow
pub fn swap(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
    let b = stack.pop()?;
    let a = stack.pop()?;
    stack.push(b);
    stack.push(a);
    Ok(())
}

/// Copies the second-to-top element of the stack and pushes it on top.
///
/// For example, if the stack has values `[1, 2, 3]` and OVER is called, the new stack
/// will contain `[1, 2, 3, 2]`.
///
/// # Errors
///
/// - [`Error::StackUnderflow`] - the stack did not have at least two values.
///
/// [`Error::StackUnderflow`]: crate::Error::StackUnderflow
pub fn over(stack: &mut Stack, _dictionary: &Words) -> Result<()> {
    let top = stack.pop()?;
    let prev = stack.peek()?;
    stack.push(top);
    stack.push(prev);
    Ok(())
}

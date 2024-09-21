//! Definition of built-in Forth words.

pub mod arith;
pub mod stack;

use crate::word::{box_fn_op, Words};

/// Adds all built-in [`Word`](crate::word::Word) to the given [word map](Words).
pub fn add_builtin_words(words: &mut Words) {
    words.insert("+".into(), box_fn_op(arith::plus));
    words.insert("-".into(), box_fn_op(arith::minus));
    words.insert("*".into(), box_fn_op(arith::times));
    words.insert("/".into(), box_fn_op(arith::quot));
    words.insert("DUP".into(), box_fn_op(stack::dup));
    words.insert("DROP".into(), box_fn_op(stack::drop));
    words.insert("SWAP".into(), box_fn_op(stack::swap));
    words.insert("OVER".into(), box_fn_op(stack::over));
}

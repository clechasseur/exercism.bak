use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(first: &[T], second: &[T]) -> Comparison {
    let (cmp, result) = match first.len().cmp(&second.len()) {
        Ordering::Equal => (first == second, Comparison::Equal),
        Ordering::Less => (first.sublist_of(second), Comparison::Sublist),
        Ordering::Greater => (second.sublist_of(first), Comparison::Superlist),
    };
    if cmp {
        result
    } else {
        Comparison::Unequal
    }
}

trait SublistHelper<T: PartialEq> {
    fn sublist_of(&self, other: &[T]) -> bool;
}

impl<T: PartialEq> SublistHelper<T> for [T] {
    fn sublist_of(&self, other: &[T]) -> bool {
        self.is_empty() || other.windows(self.len()).any(|w| w == self)
    }
}

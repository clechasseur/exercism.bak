// This file contains a simple implementation of `group_by`, a method
// that is only currently available in nightly Rust:
// https://doc.rust-lang.org/std/primitive.slice.html#method.group_by

pub struct GroupBy<'a, T, F>
where
    F: FnMut(&T, &T) -> bool
{
    remaining_items: &'a [T],
    compare_fn: F,
}

pub trait ClGroupBy<'a, T>
{
    fn cl_group_by<F>(&self, compare_fn: F) -> GroupBy<'a, T, F>
    where
        F: FnMut(&T, &T) -> bool;
}

impl<'a, T> ClGroupBy<'a, T> for &'a [T]
{
    fn cl_group_by<F>(&self, compare_fn: F) -> GroupBy<'a, T, F>
    where
        F: FnMut(&T, &T) -> bool
    {
        GroupBy::new(self, compare_fn)
    }
}

impl<'a, T, F> GroupBy<'a, T, F>
where
    F: FnMut(&T, &T) -> bool
{
    fn new(items: &'a [T], compare_fn: F) -> Self {
        Self {
            remaining_items: items,
            compare_fn,
        }
    }
}

impl<'a, T, F> Iterator for GroupBy<'a, T, F>
where
    F: FnMut(&T, &T) -> bool
{
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        let first = self.remaining_items.first()?;
        let mut i = 1;
        while i < self.remaining_items.len() && (self.compare_fn)(first, &self.remaining_items[i]) {
            i += 1;
        }

        let group = &self.remaining_items[..i];
        self.remaining_items = &self.remaining_items[i..];
        Some(group)
    }
}

impl<'a, T, F> ::std::iter::FusedIterator for GroupBy<'a, T, F>
where
    F: FnMut(&T, &T) -> bool
{
}

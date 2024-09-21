pub struct GroupBy<'a, T: 'a, F: FnMut(&T, &T) -> bool> {
    s: &'a [T],
    len: usize,
    i: usize,
    f: F,
}

pub trait ClGroupBy<'a, T: 'a> {
    fn cl_group_by<F: FnMut(&T, &T) -> bool>(&self, f: F) -> GroupBy<'a, T, F>;
}

impl<'a, T: 'a> ClGroupBy<'a, T> for &'a [T] {
    fn cl_group_by<F: FnMut(&T, &T) -> bool>(&self, f: F) -> GroupBy<'a, T, F> {
        GroupBy::new(self, f)
    }
}

impl<'a, T: 'a, F: FnMut(&T, &T) -> bool> GroupBy<'a, T, F> {
    fn new(s: &'a [T], f: F) -> Self {
        GroupBy {
            s,
            len: s.len(),
            i: 0,
            f,
        }
    }
}

impl<'a, T: 'a, F: FnMut(&T, &T) -> bool> Iterator for GroupBy<'a, T, F> {
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        match self.i {
            end if end == self.len => None,
            beg => {
                let cur = &self.s[beg];
                self.i += 1;
                while self.i < self.len && (self.f)(cur, &self.s[self.i]) {
                    self.i += 1;
                }
                Some(&self.s[beg..self.i])
            }
        }
    }
}

impl<'a, T: 'a, F: FnMut(&T, &T) -> bool> ::std::iter::FusedIterator for GroupBy<'a, T, F> {}

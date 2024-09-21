pub struct GroupBy<'a, T, F>
where
    T: 'a,
    F: FnMut(&T, &T) -> bool,
{
    s: &'a [T],
    len: usize,
    i: usize,
    f: F,
}

pub trait ClGroupBy<'a, T>
where
    T: 'a,
{
    fn cl_group_by<F>(&self, f: F) -> GroupBy<'a, T, F>
    where
        F: FnMut(&T, &T) -> bool;
}

impl<'a, T> ClGroupBy<'a, T> for &'a [T]
where
    T: 'a,
{
    fn cl_group_by<F>(&self, f: F) -> GroupBy<'a, T, F>
    where
        F: FnMut(&T, &T) -> bool,
    {
        GroupBy::new(self, f)
    }
}

impl<'a, T, F> GroupBy<'a, T, F>
where
    T: 'a,
    F: FnMut(&T, &T) -> bool,
{
    fn new(s: &'a [T], f: F) -> Self {
        GroupBy {
            s,
            len: s.len(),
            i: 0,
            f,
        }
    }
}

impl<'a, T, F> Iterator for GroupBy<'a, T, F>
where
    T: 'a,
    F: FnMut(&T, &T) -> bool,
{
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

impl<'a, T, F> ::std::iter::FusedIterator for GroupBy<'a, T, F>
where
    T: 'a,
    F: FnMut(&T, &T) -> bool,
{
}

#[derive(Debug)]
pub struct CircularBuffer<T> {
    data: Vec<Option<T>>,
    head_i: usize,
    len: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let mut data = Vec::with_capacity(capacity);
        // Note: here we use `resize_with` instead of `resize` or the `vec!`
        // macro to avoid having to add a `Clone` bound on type `T`.
        data.resize_with(capacity, || None);
        Self { data, head_i: 0, len: 0 }
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn is_full(&self) -> bool {
        self.len == self.capacity()
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.is_empty() {
            Err(Error::EmptyBuffer)
        } else {
            let element = self.data[self.head_i]
                .take()
                .expect("Element in non-empty buffer should be Some");
            self.inc_head();
            self.len -= 1;
            Ok(element)
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.is_full() {
            Err(Error::FullBuffer)
        } else {
            self.overwrite(element);
            Ok(())
        }
    }

    pub fn overwrite(&mut self, element: T) {
        let i = (self.head_i + self.len) % self.capacity();
        self.data[i] = Some(element);
        if self.is_full() {
            self.inc_head();
        } else {
            self.len += 1;
        }
    }

    pub fn clear(&mut self) {
        while !self.is_empty() {
            self.read().expect("read on a non-empty buffer should work");
        }
    }

    fn inc_head(&mut self) {
        self.head_i = (self.head_i + 1) % self.capacity();
    }
}

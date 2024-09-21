pub struct CircularBuffer<T> {
    data: Vec<Option<T>>,
    len: usize,
    read_i: usize,
    write_i: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let mut data = Vec::with_capacity(capacity);
        // Note: here we use `resize_with` instead of `resize` to avoid
        // having to add a `Clone` bound on type `T`.
        data.resize_with(capacity, || None);
        Self { data, len: 0, read_i: 0, write_i: 0 }
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
            let capacity = self.capacity();
            self.len -= 1;
            Ok(self.data[post_inc_mod(&mut self.read_i, capacity)]
                .take()
                .expect("Element in non-empty buffer should be Some"))
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
        let capacity = self.capacity();
        if self.is_full() {
            post_inc_mod(&mut self.read_i, capacity);
        } else {
            self.len += 1;
        }
        self.data[post_inc_mod(&mut self.write_i, capacity)] = Some(element);
    }

    pub fn clear(&mut self) {
        for elem in &mut self.data {
            *elem = None;
        }
        self.len = 0;
        self.read_i = 0;
        self.write_i = 0;
    }
}

/// Increments index `i`, constrained by `capacity`.
///
/// Returns the value if `i` **before** the increment.
fn post_inc_mod(i: &mut usize, capacity: usize) -> usize {
    let prev_p = *i;
    *i = (*i + 1) % capacity;
    prev_p
}

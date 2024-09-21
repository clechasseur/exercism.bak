use std::iter::FromIterator;

pub struct SimpleLinkedList<T> {
    head: Option<Box<Node<T>>>,
    len: usize,
}

struct Node<T> {
    data: T,
    next: Option<Box<Node<T>>>,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        Self {
            head: None,
            len: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn push(&mut self, element: T) {
        let node = Box::new(Node {
            data: element,
            next: self.head.take(),
        });
        self.head = Some(node);
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|mut node| {
            let data = node.data;
            self.head = node.next.take();
            self.len -= 1;
            data
        })
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|b| &b.data)
    }

    #[must_use]
    pub fn rev(mut self) -> SimpleLinkedList<T> {
        // The easiest implementation of this involves creating a new linked list,
        // then popping items from the old list and pushing them to the new list.
        // I've included this implementation below as 'rev_in_new'.

        // This implementation is more complex - it re-chains the existing list nodes
        // in reverse order, thus avoiding allocations of new nodes. It yields better
        // performance at the cost of being harder to understand and maintain.

        // Which implementation is better depends on your goal when you write it, I guess.
        // I'll leave both implementations in there; they both solve the tests.

        if self.len > 1 {
            let mut prev = self.head.take().unwrap();
            self.head = prev.next.take();
            while let Some(mut cur) = self.head.take() {
                let next = cur.next.replace(prev);
                prev = cur;
                self.head = next;
            }
            self.head = Some(prev);
        }
        self
    }

    #[must_use]
    pub fn rev_in_new(mut self) -> SimpleLinkedList<T> {
        let mut list = Self::new();
        while let Some(element) = self.pop() {
            list.push(element);
        }
        list
    }
}

impl<T> FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = Self::new();
        for element in iter {
            list.push(element);
        }
        list
    }
}

impl<T> From<SimpleLinkedList<T>> for Vec<T> {
    fn from(mut list: SimpleLinkedList<T>) -> Vec<T> {
        let mut vec = Vec::new();
        while let Some(element) = list.pop() {
            vec.push(element);
        }
        vec.reverse();
        vec
    }
}

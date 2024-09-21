#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(first: &[T], second: &[T]) -> Comparison {
    if second.len() < first.len() {
        match sublist(second, first) {
            Comparison::Sublist => Comparison::Superlist,
            Comparison::Unequal => Comparison::Unequal,
            impossible => panic!("{:?} should not have been returned here", impossible),
        }
    } else if first.is_empty() || second.windows(first.len()).any(|w| w == first) {
        match second.len() {
            l if l == first.len() => Comparison::Equal,
            _ => Comparison::Sublist,
        }
    } else {
        Comparison::Unequal
    }
}

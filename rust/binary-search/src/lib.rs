use std::cmp::Ordering;

pub fn find<S, T>(seq: S, key: T) -> Option<usize>
where
    T: Ord,
    S: AsRef<[T]>
{
    let slice = seq.as_ref();
    let (before, after) = slice.split_at(slice.len() / 2);
    match after.first()?.cmp(&key) {
        Ordering::Equal => Some(before.len()),
        Ordering::Greater => find(before, key),
        Ordering::Less => find(&after[1..], key).map(|i| i + before.len() + 1),
    }
}

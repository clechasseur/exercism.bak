pub mod group_by;

pub trait ToFlattenedVec<T> {
    fn to_flattened_vec(&self) -> Vec<T>;
}

impl<T: Copy> ToFlattenedVec<T> for [&[T]] {
    fn to_flattened_vec(&self) -> Vec<T> {
        self.iter().map(|&c| c.to_vec()).flatten().collect()
    }
}
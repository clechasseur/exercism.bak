pub mod group_by;

pub fn flatten_to_vec<T: Copy>(slice_of_slices: &[&[T]]) -> Vec<T> {
    slice_of_slices.iter().copied().flatten().copied().collect()
}

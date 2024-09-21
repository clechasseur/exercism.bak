//! Defines a map type storing values by generation.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

/// Type used to identify a specific generation in a [`GenerationMap`].
pub type Generation = usize;

/// A wrapper for [`HashMap`] that stores values by generation.
///
/// [`GenerationMap`] internally has a [`generation`] that increases every time a new value is [`insert`]ed in the map.
/// New values do not overwrite old values, however: all values are kept along with their generation. It is then
/// possible to get a value from the map, specifying a maximum generation, using [`get_maxed`].
///
/// Because the map never actually drops any values, it is not useful as long-time storage. Furthermore, only a very
/// limited subset of operations are provided for the purpose of this exercise.
///
/// [`generation`]: GenerationMap::generation
/// [`insert`]: GenerationMap::insert
/// [`get_maxed`]: GenerationMap::get_maxed
#[derive(Debug, Clone)]
pub struct GenerationMap<K, V> {
    generation: Generation,
    data: GenerationalData<K, V>,
}

type GenerationalValue<V> = (Generation, V);
type GenerationalValues<V> = Vec<GenerationalValue<V>>;
// Note: currently, the map associates each key with a `Vec` of values by generation.
// For cases where a single value will ever be inserted in the map, it could be more
// efficient to avoid the `Vec` allocation, but it makes the code more complex;
// benchmarking would also be required to prove it's much more efficient.
type GenerationalData<K, V> = HashMap<K, GenerationalValues<V>>;

impl<K, V> GenerationMap<K, V> {
    /// Creates a new, empty [`GenerationMap`].
    pub fn new() -> Self {
        Self { generation: 0, data: GenerationalData::new() }
    }

    /// Returns the current map generation.
    ///
    /// This value will increase every time a new value is [`insert`]ed in the map.
    ///
    /// A map starts at generation 0. The first [`insert`] will be associated with generation 1; thus, generation 0
    /// refers to the state of the map when it was empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// assert_eq!(0, map.generation());
    ///
    /// map.insert(42, 23);
    /// assert_eq!(1, map.generation());
    /// ```
    ///
    /// [`insert`]: GenerationMap::insert
    pub fn generation(&self) -> Generation {
        self.generation
    }

    /// Returns the number of items in the map.
    ///
    /// This does not count multiple values for a key; it only counts the unique keys.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// assert_eq!(0, map.len());
    ///
    /// map.insert(42, 23);
    /// map.insert(7, 11);
    /// assert_eq!(2, map.len());
    ///
    /// map.insert(42, 66);
    /// assert_eq!(2, map.len()); // Redefining the same key does not increment length
    /// ```
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Checks if the map is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// assert!(map.is_empty());
    ///
    /// map.insert(42, 23);
    /// assert!(!map.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<K, V> GenerationMap<K, V>
where
    K: Eq + Hash,
{
    /// Inserts a new value in the map, associating it with the given key. Returns its generation.
    ///
    /// Before the new value is inserted, the map's [`generation`](GenerationMap::generation) is incremented.
    /// The new value will be associated with this new generation.
    ///
    /// # Panics
    ///
    /// This method will panic if the map's generation cannot be incremented. (In practice this should
    /// never happen as the memory would've been filled before that)
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// let initial_generation = map.generation();
    ///
    /// let first_generation = map.insert(42, 23);
    /// assert_eq!(initial_generation + 1, first_generation);
    ///
    /// let second_generation = map.insert(7, 11);
    /// assert_eq!(first_generation + 1, second_generation);
    ///
    /// let third_generation = map.insert(42, 66);
    /// assert_eq!(second_generation + 1, third_generation); // Redefining the same key still increments generation
    /// ```
    pub fn insert(&mut self, k: K, v: V) -> Generation {
        self.generation = self
            .generation
            .checked_add(1)
            .expect("GenerationMap is full");
        self.data.entry(k).or_default().push((self.generation, v));
        self.generation
    }

    /// Returns a reference to the value associated with the given key, if any.
    ///
    /// If multiple values have been associated with the key, the latest one (e.g. the one with the highest
    /// generation) is returned.
    ///
    /// If the map doesn't contain the key, [`None`] is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// assert!(map.get(&42).is_none());
    ///
    /// map.insert(42, 23);
    /// assert_eq!(Some(&23), map.get(&42));
    ///
    /// map.insert(7, 11);
    /// assert_eq!(Some(&11), map.get(&7));
    ///
    /// map.insert(42, 66);
    /// assert_eq!(Some(&66), map.get(&42));
    /// ```
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.get_maxed(k, self.generation)
    }

    /// Returns a reference to the value associated with the given key, if any, taking a maximum generation into account.
    ///
    /// If multiple values have been associated with the key, the one with the highest generation that is _less than or
    /// equal to_ `max_generation` is returned.
    ///
    /// If the map doesn't contain the key, or if the values associated with the key are more recent than `max_generation`,
    /// [`None`] is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use forth::generation_map::GenerationMap;
    ///
    /// let mut map = GenerationMap::new();
    /// assert!(map.get(&42).is_none());
    ///
    /// let first_generation = map.insert(42, 23);
    /// assert_eq!(Some(&23), map.get_maxed(&42, first_generation));
    ///
    /// let second_generation = map.insert(42, 66);
    /// assert_eq!(Some(&66), map.get_maxed(&42, second_generation));
    /// assert_eq!(Some(&23), map.get_maxed(&42, first_generation));
    ///
    /// let third_generation = map.insert(7, 11);
    /// assert_eq!(Some(&66), map.get_maxed(&42, third_generation));
    /// assert_eq!(Some(&66), map.get_maxed(&42, second_generation));
    /// assert_eq!(Some(&23), map.get_maxed(&42, first_generation));
    /// assert_eq!(Some(&11), map.get_maxed(&7, third_generation));
    /// assert!(map.get_maxed(&7, second_generation).is_none());
    /// assert!(map.get_maxed(&7, first_generation).is_none());
    /// ```
    pub fn get_maxed<Q>(&self, k: &Q, max_generation: Generation) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.data
            .get(k)
            .and_then(|gen_data| gen_data.iter().rfind(|&(gen, _)| gen <= &max_generation))
            .map(|(_, value)| value)
    }
}

// Note: we can't `#[derive(Default)]` here, because it would add bounds to `K` and `V`, requiring them to also be `Default`.
// This is not required however since the map will initially be empty and we never use default values.
impl<K, V> Default for GenerationMap<K, V> {
    /// Creates a new, empty [`GenerationMap`].
    ///
    /// This is similar to calling [`GenerationMap::new`].
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let map = GenerationMap::<i32, i32>::default();
        assert_eq!(0, map.generation());
        assert!(map.is_empty());
        assert_eq!(0, map.len());
    }
}

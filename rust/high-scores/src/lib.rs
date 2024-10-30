/// A list of high scores, ordered from oldest to newest.
/// 
/// Works with any type `T`, as long as it implements [`AsRef<[u32]>`].
/// This is true for `Vec<u32>`, but also for slices (e.g. `[u32; N]`)
/// and slice references (e.g. `&[u32]`).
/// 
/// # Notes
/// 
/// Even though using a generic type with a trait bound allows us to
/// support both owning and borrowing scenarios, in practice this
/// might not always be advisable; for example, it doesn't work if
/// we need to mutate the scores. For immutable types however, it
/// showcases an interesting implementation choice.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct HighScores<T>(T);

impl<T> HighScores<T> {
    pub fn new(scores: T) -> Self {
        Self(scores)
    }
}

impl<T> HighScores<T>
where
    T: AsRef<[u32]>,
{
    pub fn scores(&self) -> &[u32] {
        self.0.as_ref()
    }

    pub fn latest(&self) -> Option<u32> {
        self.scores().last().copied()
    }

    pub fn personal_best(&self) -> Option<u32> {
        self.scores().iter().max().copied()
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        let mut top_three = self.scores().to_vec();
        top_three.sort_unstable_by(|a, b| b.cmp(a));
        top_three.truncate(3);
        top_three
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_with_slice() {
        let data: [u32; 3] = [42, 11, 23];
        let scores = HighScores::new(data);
        assert_eq!(scores.scores(), &[42, 11, 23]);
        assert_eq!(scores.latest(), Some(23));
        assert_eq!(scores.personal_best(), Some(42));
        assert_eq!(scores.personal_top_three(), vec![42, 23, 11]);
    }

    #[test]
    fn test_with_slice_ref() {
        let data: &[u32] = &[42, 11, 23];
        let scores = HighScores::new(data);
        assert_eq!(scores.scores(), &[42, 11, 23]);
        assert_eq!(scores.latest(), Some(23));
        assert_eq!(scores.personal_best(), Some(42));
        assert_eq!(scores.personal_top_three(), vec![42, 23, 11]);
    }

    #[test]
    fn test_with_slice_mut_ref() {
        let data: &mut [u32] = &mut [42, 11, 23];
        let scores = HighScores::new(data);
        assert_eq!(scores.scores(), &[42, 11, 23]);
        assert_eq!(scores.latest(), Some(23));
        assert_eq!(scores.personal_best(), Some(42));
        assert_eq!(scores.personal_top_three(), vec![42, 23, 11]);
    }
    
    #[test]
    fn test_with_vector() {
        let data: Vec<u32> = vec![42, 11, 23];
        let scores = HighScores::new(data);
        assert_eq!(scores.scores(), &[42, 11, 23]);
        assert_eq!(scores.latest(), Some(23));
        assert_eq!(scores.personal_best(), Some(42));
        assert_eq!(scores.personal_top_three(), vec![42, 23, 11]);
    }
}

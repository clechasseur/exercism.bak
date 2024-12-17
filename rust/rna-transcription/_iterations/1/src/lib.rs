use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dna(Rna);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rna(String);

impl Dna {
    pub fn new(dna: &str) -> Result<Self, usize> {
        Ok(Self(Rna::new_unchecked(
            dna.chars()
                .enumerate()
                .map(|(i, c)| RNA_COMPLEMENTS.get(&c).copied().ok_or(i))
                .collect::<Result<_, _>>()?,
        )))
    }

    pub fn into_rna(self) -> Rna {
        self.0
    }
}

impl Rna {
    pub fn new(rna: &str) -> Result<Self, usize> {
        Ok(Self(
            rna.chars()
                .enumerate()
                .map(|(i, c)| VALID_RNA_NUCLEOTIDES.contains(&c).then_some(c).ok_or(i))
                .collect::<Result<_, _>>()?,
        ))
    }

    fn new_unchecked(rna: String) -> Self {
        Self(rna)
    }
}

static RNA_COMPLEMENTS: LazyLock<HashMap<char, char>> =
    LazyLock::new(|| [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')].into());
static VALID_RNA_NUCLEOTIDES: LazyLock<HashSet<char>> =
    LazyLock::new(|| RNA_COMPLEMENTS.values().copied().collect());

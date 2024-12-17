use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

/// A DNA sequence.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dna<'a>(Cow<'a, str>);

/// An RNA sequence.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rna<'a>(Cow<'a, str>);

impl<'a> Dna<'a> {
    /// Returns a new DNA sequence if the provided nucleotide sequence is valid.
    ///
    /// If the sequence contains an invalid DNA nucleotide, returns [`Err`] with
    /// the index of the invalid nucleotide.
    pub fn new(dna: &'a str) -> Result<Self, usize> {
        Ok(Self(validate_nucleotides(
            dna.into(),
            &VALID_DNA_NUCLEOTIDES,
        )?))
    }

    /// Transcribes this DNA sequence into its [RNA complement](Rna).
    pub fn into_rna(self) -> Rna<'static> {
        Rna::new_unchecked(self.0.chars().map(|c| RNA_COMPLEMENTS[&c]).collect())
    }
}

impl<'a> Rna<'a> {
    /// Returns a new RNA sequence if the provided nucleotide sequence is valid.
    ///
    /// If the sequence contains an invalid RNA nucleotide, returns [`Err`] with
    /// the index of the invalid nucleotide.
    pub fn new(rna: &'a str) -> Result<Self, usize> {
        Ok(Self(validate_nucleotides(
            rna.into(),
            &VALID_RNA_NUCLEOTIDES,
        )?))
    }

    /// Returns a new RNA sequence without doing validation.
    ///
    /// Used by [`Dna`] to transcribe DNA into RNA, since a [`Dna`] sequence
    /// is always valid.
    fn new_unchecked(rna: String) -> Self {
        Self(rna.into())
    }
}

/// Map of RNA complements for every valid DNA nucleotide.
static RNA_COMPLEMENTS: LazyLock<HashMap<char, char>> =
    LazyLock::new(|| [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')].into());

/// Set of all valid DNA nucleotides.
static VALID_DNA_NUCLEOTIDES: LazyLock<HashSet<char>> =
    LazyLock::new(|| RNA_COMPLEMENTS.keys().copied().collect());

/// Set of all valid RNA nucleotides.
static VALID_RNA_NUCLEOTIDES: LazyLock<HashSet<char>> =
    LazyLock::new(|| RNA_COMPLEMENTS.values().copied().collect());

/// Validates the given nucleotide sequence.
///
/// If all nucleotides in sequence are valid (e.g. can be found in `valid_nucleotides`),
/// returns [`Ok(nucleotides)`]. Otherwise, returns [`Err`] with the index of the invalid nucleotide.
fn validate_nucleotides<'a>(
    nucleotides: Cow<'a, str>,
    valid_nucleotides: &HashSet<char>,
) -> Result<Cow<'a, str>, usize> {
    match nucleotides
        .chars()
        .position(|c| !valid_nucleotides.contains(&c))
    {
        Some(invalid_idx) => Err(invalid_idx),
        None => Ok(nucleotides),
    }
}

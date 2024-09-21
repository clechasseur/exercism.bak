mod slice_utils;

use std::cmp::Ordering;
use std::str::FromStr;
use derivative::Derivative;
use strum_macros::{EnumString, FromRepr};
use thiserror::Error;
use crate::detail::slice_utils::group_by::ClGroupBy;
use crate::detail::slice_utils::SliceUtils;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Invalid card format `{0}`")]
    InvalidCardFormat(String),
    #[error("Invalid card count `{0}` (should be 5)")]
    InvalidCardCount(usize),
}

trait OrInvalidCardFormat<T> {
    fn or_invalid_card_format(self, card_format: &str) -> T;
}

impl OrInvalidCardFormat<Error> for strum::ParseError {
    fn or_invalid_card_format(self, card_format: &str) -> Error {
        match self {
            strum::ParseError::VariantNotFound => Error::InvalidCardFormat(card_format.to_string()),
        }
    }
}

impl<T, E> OrInvalidCardFormat<Result<T, Error>> for Result<T, E>
    where
        E: OrInvalidCardFormat<Error>
{
    fn or_invalid_card_format(self, card_format: &str) -> Result<T, Error> {
        self.map_err(|e| e.or_invalid_card_format(card_format))
    }
}

#[repr(i8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, EnumString, FromRepr)]
enum CardValue {
    #[strum(serialize = "2")]
    Two,
    #[strum(serialize = "3")]
    Three,
    #[strum(serialize = "4")]
    Four,
    #[strum(serialize = "5")]
    Five,
    #[strum(serialize = "6")]
    Six,
    #[strum(serialize = "7")]
    Seven,
    #[strum(serialize = "8")]
    Eight,
    #[strum(serialize = "9")]
    Nine,
    #[strum(serialize = "10")]
    Ten,
    #[strum(serialize = "J")]
    Jack,
    #[strum(serialize = "Q")]
    Queen,
    #[strum(serialize = "K")]
    King,
    #[strum(serialize = "A")]
    Ace,
}

impl CardValue {
    pub fn next(&self) -> Self {
        match Self::from_repr((*self as i8) + 1) {
            Some(cv) => cv,
            None => Self::from_repr(0).unwrap(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, EnumString)]
enum Suit {
    #[strum(serialize = "H")]
    Hearts,
    #[strum(serialize = "S")]
    Spades,
    #[strum(serialize = "D")]
    Diamonds,
    #[strum(serialize = "C")]
    Clubs,
}

#[derive(Derivative)]
#[derivative(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Card {
    value: CardValue,
    #[derivative(PartialEq = "ignore")]
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    suit: Suit,
}

impl Card {
    pub fn new(card_s: &str) -> Result<Card, Error> {
        let (value_s, suit_s) = card_s.split_at(card_s.len() - 1);
        let value = CardValue::from_str(value_s).or_invalid_card_format(card_s)?;
        let suit = Suit::from_str(suit_s).or_invalid_card_format(card_s)?;
        Ok(Card { value, suit })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    Pair,
    TwoPairs,
    ThreeOfAKind,
    Straight,
    Flush,
    FullHouse,
    FourOfAKind,
    StraightFlush,
}

#[derive(Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord)]
pub struct Hand<'a> {
    #[derivative(PartialEq = "ignore")]
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    hand_s: &'a str,
    hand_type: HandType,
    contributing_cards: Vec<Card>,
    non_contributing_cards: Vec<Card>,
}

impl<'a> Hand<'a> {
    pub fn new(hand_s: &'a str) -> Result<Hand<'a>, Error> {
        let mut cards = hand_s.split(' ')
            .map(|card_s| Card::new(card_s))
            .collect::<Result<Vec<_>, _>>()?;
        if cards.len() != 5 {
            return Err(Error::InvalidCardCount(cards.len()))
        }
        cards.sort_unstable();

        let cards = &cards[..];
        let hand_type: Option<_>;
        let mut contributing_cards: Option<Vec<_>> = None;
        let mut non_contributing_cards: Option<Vec<_>> = None;

        if Self::is_straight(&cards) {
            if Self::is_flush(&cards) {
                hand_type = Some(HandType::StraightFlush);
            } else {
                hand_type = Some(HandType::Straight);
            }

            if Self::is_low_straight(&cards) {
                let mut cc = Vec::with_capacity(5);
                cc.push(cards[4]);
                cc.append(&mut cards[0..=3].into());
                contributing_cards = Some(cc);
            }
        } else if Self::is_flush(&cards) {
            hand_type = Some(HandType::Flush);
        } else if Self::is_full_house(&cards) {
            hand_type = Some(HandType::FullHouse);
            if cards[1] == cards[2] {
                contributing_cards = Some(cards[0..=2].into());
                non_contributing_cards = Some(cards[3..=4].into());
            } else {
                contributing_cards = Some(cards[2..=4].into());
                non_contributing_cards = Some(cards[0..=1].into());
            }
        } else {
            // Once https://doc.rust-lang.org/std/primitive.slice.html#method.group_by
            // is stabilized, we could switch to that instead.
            let mut chunks: Vec<_> = cards.cl_group_by(|a, b| a == b).collect();
            chunks.sort_unstable_by(|&c1, &c2| {
                match c1.len().cmp(&c2.len()) {
                    Ordering::Equal => c1[0].cmp(&c2[0]),
                    cmp => cmp,
                }
            });

            hand_type = match chunks.last().unwrap().len() {
                4 => Some(HandType::FourOfAKind),
                3 => Some(HandType::ThreeOfAKind),
                2 => match chunks.iter().filter(|&c| c.len() == 2).count() {
                    2 => {
                        contributing_cards = Some(chunks[(chunks.len() - 2)..chunks.len()].to_flattened_vec());
                        non_contributing_cards = Some(chunks[0].into());
                        Some(HandType::TwoPairs)
                    },
                    _ => Some(HandType::Pair),
                },
                _ => Some(HandType::HighCard),
            };

            contributing_cards = contributing_cards.or_else(|| Some(chunks.last().unwrap().to_vec()));
            non_contributing_cards = non_contributing_cards.or_else(|| Some(chunks[0..(chunks.len() - 1)].to_flattened_vec()));
        }

        let hand_type = hand_type.unwrap();
        let mut contributing_cards = contributing_cards.unwrap_or_else(|| cards.into());
        let mut non_contributing_cards = non_contributing_cards.unwrap_or_else(|| Vec::new());
        contributing_cards.reverse();
        non_contributing_cards.reverse();

        Ok(Hand {
            hand_s,
            hand_type,
            contributing_cards,
            non_contributing_cards,
        })
    }

    pub fn hand_s(&self) -> &'a str {
        self.hand_s
    }

    fn is_flush(cards: &[Card]) -> bool {
        cards.iter().all(|c| c.suit == cards[0].suit)
    }

    fn is_straight(cards: &[Card]) -> bool {
        cards.iter().skip(1).enumerate().all(|(i, c)| {
            cards[i].value.next() == c.value ||
                (i == 3 && cards[i].value == CardValue::Five && c.value == CardValue::Ace)
        })
    }

    fn is_low_straight(cards: &[Card]) -> bool {
        Self::is_straight(cards) && cards[3].value == CardValue::Five && cards[4].value == CardValue::Ace
    }

    fn is_full_house(cards: &[Card]) -> bool {
        (cards[0] == cards[1] && cards[2] == cards[3] && cards[3] == cards[4] && cards[1] != cards[2]) ||
            (cards[0] == cards[1] && cards[1] == cards[2] && cards[3] == cards[4] && cards[2] != cards[3])
    }
}

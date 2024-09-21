mod slice_utils;

use std::cmp::Ordering;
use crate::detail::slice_utils::group_by::ClGroupBy;
use crate::detail::slice_utils::flatten_to_vec;

#[derive(Debug)]
pub enum Error {
    InvalidCardFormat(String),
    InvalidCardCount(usize),
}

const CARD_VALUES: [&str; 13] = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"];
const SUITS: [&str; 4] = ["H", "S", "D", "C"];
const ACE_VALUE: usize = 14;

fn position_of(values: &[&str], value_s: &str, card_s: &str) -> Result<usize, Error> {
    match values.iter().position(|&value| value == value_s) {
        Some(pos) => Ok(pos),
        None => Err(Error::InvalidCardFormat(card_s.to_string()))
    }
}

fn next_card_value(card_value: usize) -> usize {
    match card_value + 1 {
        last if last == (CARD_VALUES.len() + 2) => 2,
        next => next,
    }
}

#[derive(Copy, Clone, Eq)]
struct Card {
    value: usize,
    suit: usize,
}

impl Card {
    pub fn new(card_s: &str) -> Result<Card, Error> {
        let (value_s, suit_s) = card_s.split_at(card_s.len() - 1);
        let value = position_of(&CARD_VALUES, value_s, card_s)? + 2;
        let suit = position_of(&SUITS, suit_s, card_s)?;

        Ok(Card {
            value,
            suit
        })
    }
}

impl PartialEq for Card {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
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

#[derive(Eq)]
pub struct Hand<'a> {
    hand_s: &'a str,
    hand_type: HandType,
    contributing_cards: Vec<Card>,
    non_contributing_cards: Vec<Card>,
}

impl<'a> Hand<'a> {
    pub fn new(hand_s: &'a str) -> Result<Hand<'a>, Error> {
        let mut cards = hand_s.split(' ')
            .map(Card::new)
            .collect::<Result<Vec<_>, _>>()?;
        if cards.len() != 5 {
            return Err(Error::InvalidCardCount(cards.len()))
        }
        cards.sort_unstable();

        let cards = &cards[..];
        let hand_type: Option<_>;
        let mut contributing_cards: Option<Vec<_>> = None;
        let mut non_contributing_cards: Option<Vec<_>> = None;

        if Self::is_straight(cards) {
            if Self::is_flush(cards) {
                hand_type = Some(HandType::StraightFlush);
            } else {
                hand_type = Some(HandType::Straight);
            }

            if Self::is_low_straight(cards) {
                let mut cc = Vec::with_capacity(5);
                cc.push(cards[4]);
                cc.append(&mut cards[0..=3].into());
                contributing_cards = Some(cc);
            }
        } else if Self::is_flush(cards) {
            hand_type = Some(HandType::Flush);
        } else if Self::is_full_house(cards) {
            hand_type = Some(HandType::FullHouse);
            if cards[1] == cards[2] {
                contributing_cards = Some(cards[0..=2].into());
                non_contributing_cards = Some(cards[3..=4].into());
            } else {
                contributing_cards = Some(cards[2..=4].into());
                non_contributing_cards = Some(cards[0..=1].into());
            }
        } else {
            let mut chunks: Vec<_> = cards.cl_group_by(Card::eq).collect();
            chunks.sort_unstable_by(|&c1, &c2| {
                c1.len().cmp(&c2.len())
                    .then_with(|| c1[0].cmp(&c2[0]))
            });

            hand_type = match chunks.last().unwrap().len() {
                4 => Some(HandType::FourOfAKind),
                3 => Some(HandType::ThreeOfAKind),
                2 => match chunks.iter().filter(|&c| c.len() == 2).count() {
                    2 => {
                        contributing_cards = Some(flatten_to_vec(&chunks[(chunks.len() - 2)..chunks.len()]));
                        non_contributing_cards = Some(chunks[0].into());
                        Some(HandType::TwoPairs)
                    },
                    _ => Some(HandType::Pair),
                },
                _ => Some(HandType::HighCard),
            };

            contributing_cards = contributing_cards.or_else(|| Some(chunks.last().unwrap().to_vec()));
            non_contributing_cards = non_contributing_cards.or_else(|| Some(flatten_to_vec(&chunks[0..(chunks.len() - 1)])));
        }

        let hand_type = hand_type.unwrap();
        let mut contributing_cards = contributing_cards.unwrap_or_else(|| cards.into());
        let mut non_contributing_cards = non_contributing_cards.unwrap_or_default();
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
            // The second part after || is for the special case of a low straight.
            // In this case, because of sort order, the Ace ends up at the end of the card slice.
            next_card_value(cards[i].value) == c.value ||
                (i == 3 && cards[i].value == 5 && c.value == ACE_VALUE)
        })
    }

    fn is_low_straight(cards: &[Card]) -> bool {
        Self::is_straight(cards) && cards[3].value == 5 && cards[4].value == ACE_VALUE
    }

    fn is_full_house(cards: &[Card]) -> bool {
        cards[0] == cards[1] &&
            cards[3] == cards[4] &&
            ((cards[2] == cards[3] && cards[1] != cards[2]) ||
                (cards[1] == cards[2] && cards[2] != cards[3]))
    }
}

impl<'a> PartialEq for Hand<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.hand_type == other.hand_type &&
            self.contributing_cards == other.contributing_cards &&
            self.non_contributing_cards == other.non_contributing_cards
    }
}

impl<'a> PartialOrd for Hand<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Hand<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.hand_type.cmp(&other.hand_type)
            .then_with(|| self.contributing_cards.cmp(&other.contributing_cards))
            .then_with(|| self.non_contributing_cards.cmp(&other.non_contributing_cards))
    }
}

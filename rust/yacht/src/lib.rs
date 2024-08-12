pub type Dice = [u8; 5];

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Category {
    Ones = 1,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}

impl Category {
    pub fn score(&self, dice: Dice) -> u8 {
        match self {
            Self::Ones | Self::Twos | Self::Threes | Self::Fours | Self::Fives | Self::Sixes => {
                self.sum_of_self(dice)
            }
            Self::FullHouse => Self::full_house(dice),
            Self::FourOfAKind => Self::four_of_a_kind(dice),
            Self::LittleStraight => Self::straight(dice, 1),
            Self::BigStraight => Self::straight(dice, 2),
            Self::Choice => dice.into_iter().sum(),
            Self::Yacht => Self::yacht(dice),
        }
    }

    fn sum_of_self(&self, dice: Dice) -> u8 {
        dice.into_iter().filter(|&d| d == (*self as u8)).sum()
    }

    fn score_if<P, S>(mut dice: Dice, sort: bool, pred: P, score_f: S) -> u8
    where
        P: FnOnce(&Dice) -> bool,
        S: FnOnce(Dice) -> u8,
    {
        if sort {
            dice.sort_unstable();
        }
        if pred(&dice) {
            score_f(dice)
        } else {
            0
        }
    }

    fn score_some_if<P>(dice: Dice, sort: bool, pred: P, score: u8) -> u8
    where
        P: FnOnce(&Dice) -> bool,
    {
        Self::score_if(dice, sort, pred, |_| score)
    }

    fn full_house(dice: Dice) -> u8 {
        Self::score_if(
            dice,
            true,
            |dice| {
                Self::is_full_house(&dice[0..=2], &dice[3..=4])
                    || Self::is_full_house(&dice[2..=4], &dice[0..=1])
            },
            |dice| dice.into_iter().sum(),
        )
    }

    fn four_of_a_kind(mut dice: Dice) -> u8 {
        dice.sort_unstable();
        if Self::all_equal(&dice[0..=3]) {
            dice[0] * 4
        } else if Self::all_equal(&dice[1..=4]) {
            dice[1] * 4
        } else {
            0
        }
    }

    fn straight(dice: Dice, starting_at: u8) -> u8 {
        Self::score_some_if(
            dice,
            true,
            |dice| dice.iter().copied().eq((starting_at..).take(5)),
            30,
        )
    }

    fn yacht(dice: Dice) -> u8 {
        Self::score_some_if(dice, false, |dice| Self::all_equal(dice), 50)
    }

    fn is_full_house(brelan: &[u8], paire: &[u8]) -> bool {
        Self::all_equal(brelan) && Self::all_equal(paire) && brelan[0] != paire[0]
    }

    fn all_equal(dice: &[u8]) -> bool {
        dice.first()
            .map(|first_dice| dice.iter().all(|d| d == first_dice))
            .unwrap_or_default()
    }
}

pub fn score(dice: Dice, category: Category) -> u8 {
    category.score(dice)
}

use std::fmt::{Display, Formatter};

const MINUTES_PER_HOUR: i32 = 60;
const MINUTES_PER_DAY: i32 = MINUTES_PER_HOUR * 24;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clock {
    minutes_in_day: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self {
            minutes_in_day: (hours * MINUTES_PER_HOUR + minutes).rem_euclid(MINUTES_PER_DAY),
        }
    }

    pub fn hours(&self) -> i32 {
        self.minutes_in_day / MINUTES_PER_HOUR
    }

    pub fn minutes(&self) -> i32 {
        self.minutes_in_day % MINUTES_PER_HOUR
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self::new(0, self.minutes_in_day + minutes)
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.hours(), self.minutes())
    }
}

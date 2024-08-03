#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration(u64);

impl Duration {
    pub fn in_seconds(&self) -> u64 {
        self.0
    }
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Self(s)
    }
}

pub trait Planet {
    fn years_during(d: &Duration) -> f64;
}

pub struct Earth;

impl Earth {
    pub const YEAR_IN_SECONDS: f64 = 31_557_600.0;
    pub const ORBITAL_PERIOD: f64 = 1.0;
}

impl Planet for Earth {
    fn years_during(d: &Duration) -> f64 {
        (d.in_seconds() as f64) / Self::YEAR_IN_SECONDS
    }
}

macro_rules! impl_planet {
    ($t:ident, $o_p:expr) => {
        pub struct $t;

        impl $t {
            pub const ORBITAL_PERIOD: f64 = $o_p;
        }

        impl Planet for $t {
            fn years_during(d: &Duration) -> f64 {
                Earth::years_during(d) / Self::ORBITAL_PERIOD
            }
        }
    }
}

impl_planet!(Mercury, 0.2408467);
impl_planet!(Venus, 0.61519726);
impl_planet!(Mars, 1.8808158);
impl_planet!(Jupiter, 11.862615);
impl_planet!(Saturn, 29.447498);
impl_planet!(Uranus, 84.016846);
impl_planet!(Neptune, 164.79132);

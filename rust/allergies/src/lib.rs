#[macro_use]
mod detail;

pub struct Allergies(u8);

define_allergens! {
    pub enum Allergen {
        Eggs,
        Peanuts,
        Shellfish,
        Strawberries,
        Tomatoes,
        Chocolate,
        Pollen,
        Cats,
    }
    const ALLERGENS;
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self(score as u8)
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.0 & (*allergen as u8) != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        ALLERGENS
            .iter()
            .filter(|allergen| self.is_allergic_to(allergen))
            .copied()
            .collect()
    }
}

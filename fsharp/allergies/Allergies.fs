module Allergies

open System

type Allergen =
    | Eggs = 0x01
    | Peanuts = 0x02
    | Shellfish = 0x04
    | Strawberries = 0x08
    | Tomatoes = 0x10
    | Chocolate = 0x20
    | Pollen = 0x40
    | Cats = 0x80

let allergicTo codedAllergies allergen =
    codedAllergies &&& (int allergen) <> 0

let list codedAllergies =
    let rec justList codedAllergies score allergens =
        let newAllergens codedAllergies candidate allergens =
            if allergicTo codedAllergies candidate then
                candidate :: allergens
            else
                allergens

        match score with
        | 0 -> allergens
        | s -> justList codedAllergies (s / 2) (newAllergens codedAllergies (enum<Allergen>(s)) allergens)

    justList codedAllergies (int Allergen.Cats) []

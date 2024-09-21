module Allergies

open System

type Allergen =
    | Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

let private reverseAllergens = [Allergen.Cats;
                                Allergen.Pollen;
                                Allergen.Chocolate;
                                Allergen.Tomatoes;
                                Allergen.Strawberries;
                                Allergen.Shellfish;
                                Allergen.Peanuts;
                                Allergen.Eggs]

let allergicTo codedAllergies allergen =
    let allergyScore allergen =
        match allergen with
        | Eggs -> 1
        | Peanuts -> 2
        | Shellfish -> 4
        | Strawberries -> 8
        | Tomatoes -> 16
        | Chocolate -> 32
        | Pollen -> 64
        | Cats -> 128

    (codedAllergies &&& allergyScore allergen) <> 0

let list codedAllergies =
    let rec justList codedAllergies candidates allergens =
        let newAllergens codedAllergies candidate allergens =
            if allergicTo codedAllergies candidate then
                candidate :: allergens
            else
                allergens

        match candidates with
        | [] -> allergens
        | candidate :: otherCandidates ->
            justList codedAllergies otherCandidates (newAllergens codedAllergies candidate allergens)

    justList codedAllergies reverseAllergens []

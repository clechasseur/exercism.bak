module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Bitwise

type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    Bitwise.and (toScore allergy) score /= 0

toList : Int -> List Allergy
toList =
    toListWith [] allAllergies 1 >> List.reverse

-- Here be helpers

-- Wait what? Check this out: https://sporto.github.io/elm-patterns/basic/type-iterator.html
nextAllergy : List Allergy -> List Allergy
nextAllergy list =
    case List.head list of
        Nothing           -> nextAllergy [Cats]
        Just Cats         -> nextAllergy (Pollen :: list)
        Just Pollen       -> nextAllergy (Chocolate :: list)
        Just Chocolate    -> nextAllergy (Tomatoes :: list)
        Just Tomatoes     -> nextAllergy (Strawberries :: list)
        Just Strawberries -> nextAllergy (Shellfish :: list)
        Just Shellfish    -> nextAllergy (Peanuts :: list)
        Just Peanuts      -> nextAllergy (Eggs :: list)
        Just Eggs         -> list

allAllergies : List Allergy
allAllergies =
    nextAllergy []

toScoreWith : List Allergy -> Int -> Allergy -> Int
toScoreWith allergies score allergy =
    case allergies of
        [] -> 0 -- Should never happen
        head :: rest ->
            if head == allergy then
                score
            else
                toScoreWith rest (Bitwise.shiftLeftBy 1 score) allergy

toScore : Allergy -> Int
toScore =
    toScoreWith allAllergies 1

toListWith : List Allergy -> List Allergy -> Int -> Int -> List Allergy
toListWith list allergies allergyScore score =
    case allergies of
        [] -> list
        allergy :: rest ->
            let
                newList =
                    if Bitwise.and allergyScore score /= 0 then
                        allergy :: list
                    else
                        list
                nextAllergyScore =
                    Bitwise.shiftLeftBy 1 allergyScore
            in
                toListWith newList rest nextAllergyScore score

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
    toListWith [] allAllergiesReversed 128

-- Here be helpers

-- Wait what? Check this out: https://sporto.github.io/elm-patterns/basic/type-iterator.html
nextAllergy : List Allergy -> List Allergy
nextAllergy list =
    case List.head list of
        Nothing           -> nextAllergy [Eggs]
        Just Eggs         -> nextAllergy (Peanuts :: list)
        Just Peanuts      -> nextAllergy (Shellfish :: list)
        Just Shellfish    -> nextAllergy (Strawberries :: list)
        Just Strawberries -> nextAllergy (Tomatoes :: list)
        Just Tomatoes     -> nextAllergy (Chocolate :: list)
        Just Chocolate    -> nextAllergy (Pollen :: list)
        Just Pollen       -> nextAllergy (Cats :: list)
        Just Cats         -> list

allAllergiesReversed : List Allergy
allAllergiesReversed =
    nextAllergy []

toScoreWith : List Allergy -> Int -> Allergy -> Int
toScoreWith allergies score allergy =
    case allergies of
        [] -> 0 -- Should never happen
        head :: rest ->
            if head == allergy then
                score
            else
                toScoreWith rest (Bitwise.shiftRightBy 1 score) allergy

toScore : Allergy -> Int
toScore =
    toScoreWith allAllergiesReversed 128

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
                    Bitwise.shiftRightBy 1 allergyScore
            in
                toListWith newList rest nextAllergyScore score

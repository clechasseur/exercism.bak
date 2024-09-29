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


toScore : Allergy -> Int
toScore allergy =
    case allergy of
        Eggs         -> 1
        Peanuts      -> 2
        Shellfish    -> 4
        Strawberries -> 8
        Tomatoes     -> 16
        Chocolate    -> 32
        Pollen       -> 64
        Cats         -> 128


allergies : List Allergy
allergies =
    [ Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats ]


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    Bitwise.and (toScore allergy) score /= 0


allergyInScore : Int -> Allergy -> Bool
allergyInScore score allergy =
    isAllergicTo allergy score


toList : Int -> List Allergy
toList score =
    allergies
    |> List.filter (allergyInScore score)

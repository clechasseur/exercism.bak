module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6

let score category (dice: Die list) =
    let dice = dice |> List.map int

    let rec allEqual dice =
        match dice with
        | a :: b :: t when a = b -> allEqual (b :: t)
        | _ :: _ :: _ -> false
        | _ -> true

    let die value dice =
        dice |> List.filter ((=) value) |> List.sum

    let fullHouse dice =
        let isFullHouse a b c d e =
            (allEqual [a; b; c] && d = e && c <> d) || (a = b && allEqual [c; d; e] && b <> c)

        match List.sort dice with
        | [a; b; c; d; e] when isFullHouse a b c d e -> List.sum dice
        | _ -> 0

    let fourOfAKind dice =
        match List.sort dice with
        | [a; b; c; d; _] when allEqual [a; b; c; d] -> 4 * a
        | [_; b; c; d; e] when allEqual [b; c; d; e] -> 4 * b
        | _ -> 0

    let littleStraight dice =
        if List.sort dice = [1..5] then 30
        else 0

    let bigStraight dice =
        if List.sort dice = [2..6] then 30
        else 0

    let yacht dice =
        if allEqual dice then 50
        else 0

    match category with
    | Category.Ones -> die 1 dice
    | Category.Twos -> die 2 dice
    | Category.Threes -> die 3 dice
    | Category.Fours -> die 4 dice
    | Category.Fives -> die 5 dice
    | Category.Sixes -> die 6 dice
    | Category.FullHouse -> fullHouse dice
    | Category.FourOfAKind -> fourOfAKind dice
    | Category.LittleStraight -> littleStraight dice
    | Category.BigStraight -> bigStraight dice
    | Category.Choice -> List.sum dice
    | Category.Yacht -> yacht dice

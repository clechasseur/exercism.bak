module GuessingGame

[<Literal>]
let private LifeTheUniverseAndEverything = 42

let reply (guess: int): string =
    match guess with
    | LifeTheUniverseAndEverything -> "Correct"
    | n when abs(n - LifeTheUniverseAndEverything) <= 1 -> "So close"
    | n when n < LifeTheUniverseAndEverything -> "Too low"
    | _ -> "Too high"

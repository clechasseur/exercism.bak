module CarsAssemble

let carsPerHour = 221.0

let successRate (speed: int): float =
    match speed with
    | 0 -> 0.0
    | 1 | 2 | 3 | 4 -> 1.0
    | 5 | 6 | 7 | 8 -> 0.9
    | 9 -> 0.8
    | 10 -> 0.77
    | _ -> failwith "Speed must be between 0 and 10"

let productionRatePerHour (speed: int): float =
    carsPerHour * float speed * successRate speed

let workingItemsPerMinute (speed: int): int =
    int (productionRatePerHour speed / 60.0)

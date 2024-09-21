module ValentinesDay

type Approval =
    | Yes
    | No
    | Maybe

type Cuisine =
    | Korean
    | Turkish

type Genre =
    | Crime
    | Horror
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity): Approval =
    match activity with
    | Movie genre when genre = Romance -> Yes
    | Restaurant cuisine when cuisine = Korean -> Yes
    | Restaurant cuisine when cuisine = Turkish -> Maybe
    | Walk km when km < 3 -> Yes
    | Walk km when km < 5 -> Maybe
    | _ -> No

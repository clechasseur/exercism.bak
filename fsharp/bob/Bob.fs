module Bob

open System

let response (input: string) =
    let responseToSpeech (input: string) =
        let yelling input =
            let rec yellingYet input yet =
                match input with
                | [] -> yet
                | h :: t when Char.IsLower(h) -> false
                | h :: t when Char.IsUpper(h) -> yellingYet t true
                | _ :: t -> yellingYet t yet

            yellingYet (input |> Seq.toList) false

        let quizzical (input: string) =
            input.EndsWith('?')

        match (yelling input, quizzical input) with
        | (true, true) -> "Calm down, I know what I'm doing!"
        | (true, false) -> "Whoa, chill out!"
        | (false, true) -> "Sure."
        | (false, false) -> "Whatever."

    match input.Trim() with
    | "" -> "Fine. Be that way!"
    | speech -> responseToSpeech speech

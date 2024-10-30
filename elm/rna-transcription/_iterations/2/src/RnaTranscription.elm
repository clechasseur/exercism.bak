module RnaTranscription exposing (toRNA)

toRNA : String -> Result String String
toRNA dna =
    String.toList dna
        |> List.map complement
        |> combineResults
        |> Result.map String.fromList
        
complement : Char -> Result String Char
complement nucleotide =
    case nucleotide of
        'G' -> Ok 'C'
        'C' -> Ok 'G'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        invalid -> Err ("Invalid nucleotide: " ++ (String.fromChar invalid))

-- Stolen from the Result.Extra package:
-- https://package.elm-lang.org/packages/elm-community/result-extra/latest
combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])

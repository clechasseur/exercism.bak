module RnaTranscription exposing (toRNA)

import Result exposing (andThen, map)

toRNA : String -> Result String String
toRNA dna =
    let
        appender c rna =
            complement c
                |> map String.fromChar
                |> map (String.append rna)
        reducer c = andThen (appender c)
    in
        String.foldl reducer (Ok "") dna
        
complement : Char -> Result String Char
complement nucleotide =
    case nucleotide of
        'G' -> Ok 'C'
        'C' -> Ok 'G'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        invalid -> Err ("Invalid nucleotide: " ++ (String.fromChar invalid))

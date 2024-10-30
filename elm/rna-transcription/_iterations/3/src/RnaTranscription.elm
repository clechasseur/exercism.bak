module RnaTranscription exposing (toRNA)

toRNA : String -> Result String String
toRNA dna =
    case String.uncons dna of
        Just (nucleotide, rest) ->
            Result.map2 String.cons (complement nucleotide) (toRNA rest)
        Nothing ->
            Ok ""
        
complement : Char -> Result String Char
complement nucleotide =
    case nucleotide of
        'G' -> Ok 'C'
        'C' -> Ok 'G'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        invalid -> Err ("Invalid nucleotide: " ++ (String.fromChar invalid))

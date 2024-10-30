module RnaTranscription exposing (toRNA)

toRNA : String -> Result String String
toRNA dna =
    case String.uncons dna of
        Just (dnaHead, dnaTail) ->
            let
                rnaHead = complement dnaHead
                rnaTail = toRNA dnaTail
            in
                Result.map2 String.cons rnaHead rnaTail
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

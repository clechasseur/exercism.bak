module RnaTranscription exposing (toRNA)

toRNA : String -> Result String String
toRNA =
    String.toList >> toRNAList >> (Result.map String.fromList)

toRNAList : List Char -> Result String (List Char)
toRNAList dna =
    case dna of
        [] -> Ok []
        h::t -> Result.map2 (::) (complement h) (toRNAList t)

complement : Char -> Result String Char
complement nucleotide =
    case nucleotide of
        'G' -> Ok 'C'
        'C' -> Ok 'G'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        invalid -> Err ("Invalid nucleotide: " ++ (String.fromChar invalid))

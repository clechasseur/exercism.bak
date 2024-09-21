module ProteinTranslation

let private protein codon =
    match codon with
    | [| 'A'; 'U'; 'G' |] -> "Methionine"
    | [| 'U'; 'U'; 'U' |] | [| 'U'; 'U'; 'C' |] -> "Phenylalanine"
    | [| 'U'; 'U'; 'A' |] | [| 'U'; 'U'; 'G' |] -> "Leucine"
    | [| 'U'; 'C'; 'U' |] | [| 'U'; 'C'; 'C' |]
    | [| 'U'; 'C'; 'A' |] | [| 'U'; 'C'; 'G' |] -> "Serine"
    | [| 'U'; 'A'; 'U' |] | [| 'U'; 'A'; 'C' |] -> "Tyrosine"
    | [| 'U'; 'G'; 'U' |] | [| 'U'; 'G'; 'C' |] -> "Cysteine"
    | [| 'U'; 'G'; 'G' |] -> "Tryptophan"
    | [| 'U'; 'A'; 'A' |] | [| 'U'; 'A'; 'G' |] | [| 'U'; 'G'; 'A' |] -> "STOP"
    | _ -> failwith $"Invalid codon: {codon}"

let proteins (rna: string): string list =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map protein
    |> Seq.takeWhile ((<>) "STOP")
    |> Seq.toList

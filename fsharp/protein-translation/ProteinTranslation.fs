module ProteinTranslation

let proteins rna =
    let protein codon =
        match codon with
        | "AUG" -> "Methionine"
        | "UUU" | "UUC" -> "Phenylalanine"
        | "UUA" | "UUG" -> "Leucine"
        | "UCU" | "UCC"
        | "UCA" | "UCG" -> "Serine"
        | "UAU" | "UAC" -> "Tyrosine"
        | "UGU" | "UGC" -> "Cysteine"
        | "UGG" -> "Tryptophan"
        | "UAA" | "UAG" | "UGA" | "" -> "STOP"
        | _ -> failwith $"Invalid codon: {codon}"

    let rec doProteins (rna: string) proteins =
        match protein rna[0..2] with
        | "STOP" -> proteins
        | p -> doProteins rna[3..] (p :: proteins)

    doProteins rna [] |> List.rev

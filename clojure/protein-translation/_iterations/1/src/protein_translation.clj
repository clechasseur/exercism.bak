(ns protein-translation)

(def ^:private proteins-to-codons
  {"Methionine"    ["AUG"]
   "Phenylalanine" ["UUU" "UUC"]
   "Leucine"       ["UUA" "UUG"]
   "Serine"        ["UCU" "UCC" "UCA" "UCG"]
   "Tyrosine"      ["UAU" "UAC"]
   "Cysteine"      ["UGU" "UGC"]
   "Tryptophan"    ["UGG"]
   "STOP"          ["UAA" "UAG" "UGA"]})

(def ^:private codons-to-protein
  (into {} (for [[protein codons] proteins-to-codons
                 codon codons]
             [codon protein])))

(defn translate-codon [codon]
  (codons-to-protein codon))

(defn translate-rna [rna]
  (loop [[nucleotide1 nucleotide2 nucleotide3 & other-nucleotides] rna
         proteins []]
    (let [protein (translate-codon (str nucleotide1 nucleotide2 nucleotide3))]
      (if (or (nil? protein) (= protein "STOP"))
        proteins
        (recur other-nucleotides (conj proteins protein))))))

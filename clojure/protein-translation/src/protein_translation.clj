(ns protein-translation
  (:require [clojure.string :as string]))

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
  (->> rna
       (partition 3)
       (map string/join)
       (map translate-codon)
       (take-while (partial not= "STOP"))))

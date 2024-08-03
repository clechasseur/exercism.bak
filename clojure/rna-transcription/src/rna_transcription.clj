(ns rna-transcription
  (:require [clojure.string :as string]))

(def ^:private dna-to-rna
  {\G \C
   \C \G
   \T \A
   \A \U})

(defn- nucleotide-to-rna [dna-nucleotide]
  {:post [(some? %)]}
  (dna-to-rna dna-nucleotide))

(defn to-rna [dna]
  (string/join (map nucleotide-to-rna dna)))

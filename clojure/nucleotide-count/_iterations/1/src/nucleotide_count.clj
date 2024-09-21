(ns nucleotide-count)

(def valid-nucleotide? #{\A \C \G \T})

(defn- validate-nucleotide [nucleotide]
  (when-not (valid-nucleotide? nucleotide)
    (throw (IllegalArgumentException. (str "Invalid nucleotide: " nucleotide)))))

(defn- validate-strand [strand]
  (doseq [nucleotide strand]
    (validate-nucleotide nucleotide)))

(defn count-of-nucleotide-in-strand [nucleotide strand]
  (validate-nucleotide nucleotide)
  (validate-strand strand)
  (count (filter #(= nucleotide %) strand)))

(defn nucleotide-counts [strand]
  (validate-strand strand)
  (->> (frequencies strand)
       (merge {\A 0 \C 0 \G 0 \T 0})))

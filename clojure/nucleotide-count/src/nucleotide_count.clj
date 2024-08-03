(ns nucleotide-count)

(def zero-nucleotide-counts {\A 0 \C 0 \G 0 \T 0})

(defn count-of-nucleotide-in-strand [nucleotide strand]
  {:pre [(zero-nucleotide-counts nucleotide)]}
  (count (filter #(= nucleotide %) strand)))

(defn nucleotide-counts [strand]
  (merge zero-nucleotide-counts (frequencies strand)))

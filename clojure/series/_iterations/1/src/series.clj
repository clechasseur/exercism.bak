(ns series)

(defn slices [string length]
  (if (zero? length)
    '("")
    (map #(apply str %) (partition length 1 string))))

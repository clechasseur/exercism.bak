(ns flatten-array)

(defn flatten [arr]
  (loop [[first & rest :as all] arr
         flat []]
    (cond
      (empty? all)     flat
      (nil? first)     (recur rest flat)
      (seqable? first) (recur rest (apply conj flat (flatten first)))
      :else            (recur rest (conj flat first)))))

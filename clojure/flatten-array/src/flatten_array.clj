(ns flatten-array
  (:refer-clojure :exclude [flatten]))

(defn flatten [arr]
  (loop [[first & rest :as all] arr
         flat []]
    (cond
      (empty? all)     flat
      (nil? first)     (recur rest flat)
      (seqable? first) (recur (concat first rest) flat)
      :else            (recur rest (conj flat first)))))

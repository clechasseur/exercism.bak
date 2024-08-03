(ns prime-factors)

(defn of [n]
  (loop [of-n n
         div 2
         factors []]
    (cond
      (= of-n 1)             factors
      (zero? (mod of-n div)) (recur (quot of-n div) div (conj factors div))
      :else                  (recur of-n (inc div) factors))))

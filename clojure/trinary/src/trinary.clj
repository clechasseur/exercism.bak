(ns trinary)

(defn to-decimal [s]
  (if-not (re-matches #"[012]+" s)
    0
    (reduce #(+ (Character/digit %2 3) (* %1 3)) 0 s)))

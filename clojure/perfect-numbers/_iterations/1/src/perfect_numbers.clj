(ns perfect-numbers)

(defn- factor-for? [n factor]
  (zero? (mod n factor)))

(defn- factors [n]
  (filter (partial factor-for? n) (range 1 n)))

(defn- aliquot-sum [n]
  (reduce + (factors n)))

(defn classify [n]
  (when-not (pos? n)
    (throw (IllegalArgumentException. "n must be positive")))
  (let [aliquot (aliquot-sum n)]
    (cond
      (= aliquot n) :perfect
      (> aliquot n) :abundant
      :else         :deficient)))

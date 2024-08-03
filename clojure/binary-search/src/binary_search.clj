(ns binary-search)

(defn middle [coll]
  (quot (count coll) 2))

(defn search-for [element coll]
  (loop [subv (vec coll)
         offset 0]
    (when (empty? subv)
      (throw (IllegalArgumentException. (str element " not found"))))
    (let [middle (middle subv)
          cmp (compare (get subv middle) element)]
      (if (zero? cmp)
        (+ middle offset)
        (let [is-before (pos? cmp)
              from (if is-before 0 (inc middle))
              to (if is-before middle (count subv))]
          (recur (subvec subv from to) (+ offset from)))))))

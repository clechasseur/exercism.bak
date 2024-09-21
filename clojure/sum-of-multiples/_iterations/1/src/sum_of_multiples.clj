(ns sum-of-multiples)

(defn- multiple-of-some [factors n]
  (some #(zero? (mod n %)) factors))

(defn sum-of-multiples [factors limit]
  (->> (range limit)
       (filter (partial multiple-of-some factors))
       (reduce +)))

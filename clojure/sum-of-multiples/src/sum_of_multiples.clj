(ns sum-of-multiples)

(defn sum-of-multiples [factors limit]
  (->> factors
       (mapcat #(range % limit %))
       distinct
       (apply +)))

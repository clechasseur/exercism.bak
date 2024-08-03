(ns pascals-triangle)

(defn- next-row [row]
  (concat [1N] (mapv + row (rest row)) [1N]))

(def triangle (iterate next-row [1N]))

(defn row [n]
  (nth triangle (dec n)))

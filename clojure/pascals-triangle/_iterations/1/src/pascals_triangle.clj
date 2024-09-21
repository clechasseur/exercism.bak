(ns pascals-triangle)

(defn- pascal [prev-row n pos]
  (cond
    (zero? pos)     1N
    (= pos (dec n)) 1N
    :else           (+ (get prev-row (dec pos)) (get prev-row pos))))

(defn- next-row [prev-row]
  (let [n (inc (count prev-row))]
    (mapv (partial pascal prev-row n) (range n))))

(defn- triangle-seq [row]
  (lazy-seq (cons row (triangle-seq (next-row row)))))

(def triangle (triangle-seq [1N]))

(defn row [n]
  (nth triangle (dec n)))

(ns sieve)

(defn- mark-if-factor [prime [number mark]]
  (if (and (nil? mark) (zero? (mod number prime)))
    [number (= number prime)]
    [number mark]))

(defn sieve [n]
  (let [unmarked-numbers (map #(vector % nil) (range 2 (inc n)))]
    (loop [numbers unmarked-numbers]
      (let [next-prime (some (fn [[n m]] (when (nil? m) n)) numbers)]
        (if-not next-prime
          (map first (filter (comp true? second) numbers))
          (recur (map (partial mark-if-factor next-prime) numbers)))))))

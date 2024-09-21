(ns nth-prime)

(defn prime?
  "Determines if n is prime."
  [n]
  (or (= n 2) (every? #(not= (mod n %) 0) (range 2 (inc (Math/sqrt n))))))

(defn nth-prime
  "Returns the prime number in the nth position."
  [n]
  (when (not (pos? n))
    (throw (IllegalArgumentException.)))
  (->> (iterate inc 2)
       (filter prime?)
       (take n)
       last))

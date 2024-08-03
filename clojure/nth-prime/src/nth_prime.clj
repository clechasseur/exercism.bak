(ns nth-prime)

(defn prime?
  "Determines if n is prime."
  [n]
  (not-any? #(zero? (mod n %)) (range 3 (inc (Math/sqrt n)))))

(def primes
  "A lazy sequence of all prime numbers."
  (concat '(2)
          (->> (iterate (partial + 2) 3)
               (filter prime?))))

(defn nth-prime
  "Returns the prime number in the nth position."
  [n]
  (when-not (pos? n)
    (throw (IllegalArgumentException.)))
  (nth primes (dec n)))

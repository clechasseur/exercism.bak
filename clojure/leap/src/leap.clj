(ns leap)

(defn- divisible-by? [n div]
  (zero? (mod n div)))

(defn leap-year? [year]
  (condp #(divisible-by? %2 %1) year
    400 true
    100 false
    4   true
    false))

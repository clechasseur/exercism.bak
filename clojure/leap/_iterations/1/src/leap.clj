(ns leap)

(defn- divisible-by? [n div]
  (zero? (mod n div)))

(defn leap-year? [year]
  (or
    (divisible-by? year 400)
    (and
      (divisible-by? year 4)
      (not (divisible-by? year 100)))))

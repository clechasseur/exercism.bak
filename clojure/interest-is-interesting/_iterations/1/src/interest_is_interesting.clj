(ns interest-is-interesting)

(defn abs
  "Returns the absolute value of n (available in Clojure 1.11.10 apparently)"
  [n]
  (max n (-' n)))

(defn interest-rate
  "Calculates the annual interest rate for the specified balance"
  [balance]
  (condp <= balance
    5000.0M 2.475
    1000.0M 1.621
    0.0M    0.5
    -3.213))

(defn annual-balance-update
  "Calculates the updated balance after applying annual interest rate"
  [balance]
  (-> (interest-rate balance)
      abs
      bigdec
      (/ 100.0M)
      (+ 1.0M)
      (* balance)))

(defn amount-to-donate
  "Calculates the amount to donate to charity for the given balance"
  [balance tax-free-percentage]
  (-> tax-free-percentage
      (/ 100)
      (* balance)
      (* 2)
      int
      (max 0)))
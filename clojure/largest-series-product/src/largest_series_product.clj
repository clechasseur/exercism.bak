(ns largest-series-product)

(defn largest-product [n digits]
  (when-not (re-matches #"\d*" digits)
    (throw (IllegalArgumentException. "Invalid digit")))
  (when-not (<= n (count digits))
    (throw (IllegalArgumentException. "Span must be <= number of digits")))
  (if (zero? n)
    1
    (->> digits
         (map #(Character/digit ^Character % 10))
         (partition n 1)
         (map (partial reduce *))
         (apply max))))

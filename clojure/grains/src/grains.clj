(ns grains)

(defn square [n]
  (loop [pos (dec n)
         result 1N]
    (if (zero? pos)
      result
      (recur (dec pos) (* 2 result)))))

(defn total []
  (dec (square 65)))

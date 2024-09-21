(ns collatz-conjecture)

(defn- collatz-next [num]
  (if (even? num)
    (/ num 2)
    (inc (* num 3))))

(defn collatz [num]
  (when (not (pos? num))
    (throw (IllegalArgumentException. "num must be >= 1")))
  (loop [steps 0
         n num]
    (if (= n 1)
      steps
      (recur (inc steps) (collatz-next n)))))

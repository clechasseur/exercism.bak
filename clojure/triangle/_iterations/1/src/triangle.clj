(ns triangle)

(defn- valid? [[a b c :as sides]]
  (and
    (every? pos? sides)
    (<= c (+ a b))))

(defn- is? [a b c fn]
  (let [sides (sort [a b c])]
    (and (valid? sides) (fn sides))))

(defn equilateral? [a b c]
  (is? a b c (partial apply =)))

(defn isosceles? [a b c]
  (is? a b c (fn [[x y z]] (or (= x y) (= y z)))))

(defn scalene? [a b c]
  (is? a b c (fn [[x y z]] (and (not= x y) (not= y z)))))

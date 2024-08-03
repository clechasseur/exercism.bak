(ns say
  (:require [clojure.string :as string]))

(def ^:private ones
  ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight", "nine", "ten"
   "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def ^:private tens
  ["twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def ^:private scales
  [nil "thousand" "million" "billion"])

(defn- divmod [num div]
  ((juxt quot rem) num div))

(defn- conj-if [coll test x & xs]
  (if test (apply conj coll x xs) coll))

(defn- less-than-20 [num]
  (get ones num))

(defn- less-than-100 [num]
  (if (< num 20)
    (less-than-20 num)
    (let [[t l] (divmod num 10)]
      (as-> [(get tens (- t 2))] $
            (conj-if $ (pos? l) (less-than-20 l))
            (string/join "-" $)))))

(defn- less-than-1000 [num]
  (let [[h l] (divmod num 100)]
    (as-> [] $
          (conj-if $ (pos? h) (less-than-100 h) "hundred")
          (conj-if $ (pos? l) (less-than-100 l))
          (string/join " " $))))

(defn- parts [num]
  (loop [r num
         p []]
    (if (zero? r)
      p
      (let [[r n] (divmod r 1000)]
        (recur r (conj p (less-than-1000 n)))))))

(defn number [num]
  (when-not (< -1 num 1000000000000)
    (throw (IllegalArgumentException. "num must be between 0 and 999999999999")))
  (if (zero? num)
    (get ones 0)
    (->> (parts num)
         (#(map vector % scales))
         (filter #(not (string/blank? (first %))))
         reverse
         flatten
         (filter some?)
         (string/join " "))))

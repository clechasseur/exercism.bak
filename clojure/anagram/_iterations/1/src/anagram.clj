(ns anagram
  (:require [clojure.string :refer [lower-case]]))

(defn- anagram? [word candidate]
  (and (not (= word candidate)) (= (sort word) (sort candidate))))

(defn anagrams-for [word prospect-list]
  (let [lower-word (lower-case word)]
    (filter #(anagram? lower-word (lower-case %)) prospect-list)))

(ns scrabble-score
  (:require [clojure.string :as string]))

(def ^:private values-for-letters
  {1 [\A \E \I \O \U \L \N \R \S \T]
   2 [\D \G]
   3 [\B \C \M \P]
   4 [\F \H \V \W \Y]
   5 [\K]
   8 [\J \X]
   10 [\Q \Z]})

(def ^:private letter-values
  (into {} (for [[score letters] values-for-letters
                 letter letters]
             [letter score])))

(defn score-word [word]
  (->> word
       string/upper-case
       (map letter-values)
       (reduce +)))

(defn score-letter [letter]
  (score-word letter))

(ns acronym
  (:require [clojure.string :refer [join upper-case]]))

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  (->> phrase
       (re-seq #"([A-Za-z])[A-Z]*[a-z]*")
       (map second)
       (map upper-case)
       join))

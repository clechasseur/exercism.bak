(ns acronym
  (:require [clojure.string :as string]))

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  (->> phrase
       (re-seq #"([A-Za-z])[A-Z]*[a-z]*")
       (map second)
       (map string/upper-case)
       (apply str)))

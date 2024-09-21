(ns proverb
  (:require [clojure.string :as string]))

(defn- line [[want lost]]
  (str "For want of a " want " the " lost " was lost."))

(defn- last-line [want]
  (str "And all for the want of a " want "."))

(defn recite [[first-piece :as pieces]]
  (if-not first-piece
    ""
    (as-> pieces $
          (partition 2 1 $)
          (mapv line $)
          (conj $ (last-line first-piece))
          (string/join "\n" $))))

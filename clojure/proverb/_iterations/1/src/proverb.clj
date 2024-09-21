(ns proverb
  (:require [clojure.string :as string]))

(defn- line [[want lost]]
  (str "For want of a " want " the " lost " was lost."))

(defn recite [[first-piece :as pieces]]
  (if-not first-piece
    ""
    (->> pieces
         (partition 2 1)
         (mapv line)
         (#(conj % (str "And all for the want of a " first-piece ".")))
         (string/join "\n"))))

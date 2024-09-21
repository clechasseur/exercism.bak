(ns etl
  (:require [clojure.string :as string]))

(defn keys-to-score [[score keys]]
  (map #(vector (string/lower-case %) score) keys))

(defn transform [src]
  (->> src
       (map keys-to-score)
       (reduce #(into %1 %2) {})))

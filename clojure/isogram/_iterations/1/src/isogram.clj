(ns isogram
  (:require [clojure.string :as string]))

(defn isogram? [s]
  (let [cleaned-up (string/lower-case (string/replace s #"[\s-]+", ""))]
    (= (count (distinct cleaned-up)) (count cleaned-up))))

(ns isogram
  (:require [clojure.string :as string]))

(defn isogram? [s]
  (apply distinct? (string/lower-case (string/replace s #"[\s-]+" ""))))

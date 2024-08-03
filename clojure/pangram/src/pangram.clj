(ns pangram
  (:require [clojure.string :as string]))

(defn pangram? [phrase]
  (->> phrase
       string/lower-case
       (re-seq #"[a-z]")
       distinct
       count
       (= 26)))

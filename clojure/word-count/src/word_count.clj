(ns word-count
  (:require [clojure.string :as string]))

(defn word-count [phrase]
  (->> phrase
       string/lower-case
       (re-seq #"\d+|[a-z]+(?:'[a-z]+)?")
       ;; this is almost cheating
       frequencies))

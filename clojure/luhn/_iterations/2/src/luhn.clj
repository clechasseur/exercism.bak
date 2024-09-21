(ns luhn
  (:require [clojure.string :as string]))

(defn- mul2-second [[first second]]
  (let [doubled-second (* 2 (or second 0))]
    [first (if (> doubled-second 9) (- doubled-second 9) doubled-second)]))

(defn valid? [s]
  (let [sanitized (string/replace s " " "")]
    (boolean
      (when (re-matches #"\d{2,}" sanitized)
        (->> sanitized
             (map #(Character/digit % 10))
             reverse
             (partition-all 2)
             (map mul2-second)
             flatten
             (apply +)
             (#(zero? (mod % 10))))))))

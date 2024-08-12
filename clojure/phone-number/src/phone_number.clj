(ns phone-number
  (:require [clojure.string :as string]))

(defn- parse [num-string]
  (let [cleaned-num-string (string/replace num-string #"\D+" "")
        match (re-matches #"1?([2-9]\d{2})([2-9]\d{2})(\d{4})" cleaned-num-string)
        [area-code exchange-code subscriber-number] (rest match)]
    (sorted-map
      :area-code (or area-code "000")
      :exchange-code (or exchange-code "000")
      :subscriber-number (or subscriber-number "0000"))))

(defn number [num-string]
  (string/join (vals (parse num-string))))

(defn area-code [num-string]
  (:area-code (parse num-string)))

(defn pretty-print [num-string]
  (let [parsed (parse num-string)]
    (str "(" (:area-code parsed) ") " (:exchange-code parsed) "-" (:subscriber-number parsed))))

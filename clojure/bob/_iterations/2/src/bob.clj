(ns bob
  (:require [clojure.string :refer [trim ends-with?]]))

(defn- yell? [s]
  (and (re-find #"[A-Z]" s) (nil? (re-find #"[a-z]" s))))

(defn- question? [s]
  (ends-with? s "?"))

(defn response-for [s]
  (let [trimmed (trim s)
        is-yelling (yell? trimmed)
        is-question (question? trimmed)]
    (cond
      (and is-yelling is-question) "Calm down, I know what I'm doing!"
      is-yelling                   "Whoa, chill out!"
      is-question                  "Sure."
      (empty? trimmed)             "Fine. Be that way!"
      :else                        "Whatever.")))

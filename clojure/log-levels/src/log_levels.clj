(ns log-levels
  (:require [clojure.string :as str]))

(def log-re #"^\[(INFO|WARNING|ERROR)\]: ((?:.|\r|\n)*)$")

(defn message
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  [s]
  (->> s
       (re-matches log-re)
       rest
       second
       str/trim))

(defn log-level
  "Takes a string representing a log line
   and returns its level in lower-case."
  [s]
  (->> s
       (re-matches log-re)
       rest
       first
       str/lower-case))

(defn reformat
  "Takes a string representing a log line and formats it
   with the message first and the log level in parentheses."
  [s]
  (str (message s) " (" (log-level s) ")"))

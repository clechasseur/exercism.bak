(ns pig-latin
  (:require [clojure.string :as string]))

(defn translate [s]
  (string/replace s #"\b((?:[^aeiouq\s]*qu)|(?:(?!xr|yt)[^aeiou\s][^aeiouy\s]*)?)(\S+)\b" "$2$1ay"))

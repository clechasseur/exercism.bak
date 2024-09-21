(ns atbash-cipher
  (:require [clojure.string :as string]))

(defn- encode-char [^Character character]
  (if (Character/isLetter character)
    (char (+ (int \a) (- (int \z) (int character))))
    character))

(defn encode [plaintext]
  (->> (string/replace plaintext #"\W+" "")
       string/lower-case
       (map encode-char)
       (partition-all 5)
       (map string/join)
       (string/join " ")))

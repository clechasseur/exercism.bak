(ns crypto-square
  (:require [clojure.string :as string]))

(defn normalize-plaintext [plaintext]
  (string/replace (string/lower-case plaintext) #"[^\da-z]+" ""))

(defn- modded-square-size [fn normalized-text]
  (int (fn (Math/sqrt (double (count normalized-text))))))

(defn square-size [normalized-text]
  (modded-square-size #(Math/ceil %) normalized-text))

(defn- sized-segments [segment-size normalized-text]
  (mapv (partial apply str) (partition-all segment-size normalized-text)))

(defn plaintext-segments [plaintext]
  (let [normalized-plaintext (normalize-plaintext plaintext)
        size (square-size normalized-plaintext)]
    (sized-segments size normalized-plaintext)))

(defn- pad-if-necessary [cols segment]
  (apply str segment (repeat (- cols (count segment)) " ")))

(defn- normalize-segments [[first-segment :as segments]]
  (mapv (partial pad-if-necessary (count first-segment)) segments))

(defn- untrimmed-ciphertext [plaintext]
  (let [segments (plaintext-segments plaintext)
        normalized-segments (normalize-segments segments)]
    (string/join (apply map str normalized-segments))))

(defn ciphertext [plaintext]
  (string/replace (untrimmed-ciphertext plaintext) " " ""))

(defn- normalized-segment-size [untrimmed-ciphertext]
  (modded-square-size #(Math/floor %) untrimmed-ciphertext))

(defn normalize-ciphertext [plaintext]
  (let [cipher (untrimmed-ciphertext plaintext)
        size (normalized-segment-size cipher)
        segments (sized-segments size cipher)]
    (string/join " " segments)))

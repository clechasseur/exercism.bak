(ns run-length-encoding
  (:require [clojure.string :as string]))

(defn- encode-match [[sequence character]]
  (str (count sequence) character))

(defn- decode-match [[_ run-length character]]
  (string/join (repeat (Integer/parseInt run-length) character)))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (string/replace plain-text #"(\D)\1+" encode-match))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (string/replace cipher-text #"(\d+)(\D)" decode-match))

(ns rotational-cipher
  (:require [clojure.string :as string]))

(defn- rotate-letter [[letter] key a]
  (str (char (+ (int a) (mod (+ (- (int letter) (int a)) key) 26)))))

(def ^:private rotate-replacements
  [[#"[A-Z]" #(rotate-letter %1 %2 \A)]
   [#"[a-z]" #(rotate-letter %1 %2 \a)]])

(defn rotate [text key]
  (reduce
    (fn [txt [re f]] (string/replace txt re #(f % key)))
    text
    rotate-replacements))

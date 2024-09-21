(ns isbn-verifier
  (:require [clojure.string :as string]))

(defn- isbn-digit-to-int [isbn-digit]
  (if (= isbn-digit \X)
    10
    (- (int isbn-digit) (int \0))))

(defn- isbn-to-int [compact-isbn]
  (->> compact-isbn
       (map isbn-digit-to-int)
       (map * (range 10 -1 -1))
       (reduce +)))

(defn isbn? [isbn]
  (let [compact-isbn (string/replace isbn "-" "")]
    (if-not (re-matches #"\d{9}[\dX]" compact-isbn)
      false
      (= (mod (isbn-to-int compact-isbn) 11) 0))))

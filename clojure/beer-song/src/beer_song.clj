(ns beer-song
  (:require [clojure.string :as string]))

(defn- pluralize-if [test word]
  (if test (str word "s") word))

(defn- bottles-of-beer [num]
  (str (if (zero? num) "no more" num)
       (pluralize-if (not= 1 num) " bottle")
       " of beer"))

(defn- action [num]
  (if (zero? num)
    "Go to the store and buy some more"
    (str "Take " (if (= 1 num) "it" "one") " down and pass it around")))

(defn verse
  "Returns the nth verse of the song."
  [num]
  (let [bottles (bottles-of-beer num)
        next-bottles (bottles-of-beer (mod (dec num) 100))
        action (action num)]
    (str (string/capitalize bottles) " on the wall, " bottles ".\n"
         action ", " next-bottles " on the wall.\n")))

(defn sing
  "Given a start and an optional end, returns all verses in this interval. If
  end is not given, the whole song from start is sung."
  ([start]
   (sing start 0))
  ([start end]
   (->> (range start (dec end) -1)
        (map verse)
        (string/join "\n"))))

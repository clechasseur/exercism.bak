(ns space-age)

(defn on-earth [age-in-s]
  (/ (float age-in-s) 31557600))

(defn on-mercury [age-in-s]
  (/ (on-earth age-in-s) 0.2408467))

(defn on-venus [age-in-s]
  (/ (on-earth age-in-s) 0.61519726))

(defn on-mars [age-in-s]
  (/ (on-earth age-in-s) 1.8808158))

(defn on-jupiter [age-in-s]
  (/ (on-earth age-in-s) 11.862615))

(defn on-saturn [age-in-s]
  (/ (on-earth age-in-s) 29.447498))

(defn on-uranus [age-in-s]
  (/ (on-earth age-in-s) 84.016846))

(defn on-neptune [age-in-s]
  (/ (on-earth age-in-s) 164.79132))

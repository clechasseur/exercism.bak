(ns raindrops)

(defn- if-factor [num factor value]
  (if (zero? (mod num factor))
    value
    ""))

(defn- pling [num]
  (if-factor num 3 "Pling"))

(defn- plang [num]
  (if-factor num 5 "Plang"))

(defn- plong [num]
  (if-factor num 7 "Plong"))

(defn convert [num]
  (let [result (str (pling num) (plang num) (plong num))]
    (if (empty? result)
      (str num)
      result)))

(ns wordy)

(def ^:private ops
  {"plus" +
   "minus" -
   "multiplied by" *
   "divided by" quot})

(def ^:private problem-re #"\s*What is (-?\d+)\s*((?:[a-z][a-z ]*[a-z]\s+-?\d+\s*)*)\s*\?\s*")
(def ^:private op-re #"([a-z][a-z ]*[a-z])\s+(-?\d+)")

(defn- parse-op-and-value [[op value]]
  (let [op-fn (get ops op)]
    (when-not op-fn
      (throw (IllegalArgumentException. "Invalid op")))
    [op-fn (Integer/parseInt value)]))

(defn- apply-op [cur-value [op-fn value]]
  (op-fn cur-value value))

(defn evaluate [problem]
  (let [[_ initial-value operations] (re-matches problem-re problem)]
    (when-not initial-value
      (throw (IllegalArgumentException. "Invalid problem")))
    (->> (re-seq op-re operations)
         (map rest)
         (map parse-op-and-value)
         (reduce apply-op (Integer/parseInt initial-value)))))

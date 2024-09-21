(ns gigasecond
  (:import (java.time LocalDateTime)))

(defn from [^Integer year ^Integer month ^Integer day]
  (let [plus-giga (.plusSeconds (LocalDateTime/of year month day 0 0) 1E9)]
    ((juxt #(.getYear %) #(.getMonthValue %) #(.getDayOfMonth %)) plus-giga)))

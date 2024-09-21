(ns accumulate)

(defn accumulate [fn [fir & rest]]
  (if (nil? fir)
    []
    (apply vector (fn fir) (accumulate fn rest))))

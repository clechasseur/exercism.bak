(ns accumulate)

(defn accumulate [fn [fir & rest]]
  (if (nil? fir)
    []
    (cons (fn fir) (accumulate fn rest))))

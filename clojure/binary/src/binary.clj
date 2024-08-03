(ns binary)

(defn to-decimal [binary]
  (loop [[digit & rest] (re-seq #"[01]" binary)
         result 0]
    (if (nil? digit)
      result
      (recur rest (+ (* result 2) (Integer/parseInt digit))))))

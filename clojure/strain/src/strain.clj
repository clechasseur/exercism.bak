(ns strain)

(defn retain [fn coll]
  ;; Note: can be implemented using for with :when modifier,
  ;; but I feel like this is cheating because it's still using
  ;; something that's provided by the standard library.
  (loop [[f & r :as all] coll
         result []]
    (if (empty? all)
      result
      (recur r (if (fn f) (conj result f) result)))))

(defn discard [fn coll]
  (retain (complement fn) coll))

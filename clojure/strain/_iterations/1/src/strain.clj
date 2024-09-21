(ns strain)

(defn retain [fn coll]
  (loop [[f & r :as all] coll
         result []]
    (if (empty? all)
      result
      (recur r (if (fn f) (conj result f) result)))))

(defn discard [fn coll]
  (retain (comp not fn) coll))

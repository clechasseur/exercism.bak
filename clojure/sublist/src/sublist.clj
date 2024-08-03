(ns sublist)

(defn- invert [result]
  (if (= result :sublist) :superlist result))

(defn classify [list1 list2]
  (let [len1 (count list1)
        len2 (count list2)]
    (cond
      (> len1 len2)                            (invert (classify list2 list1))
      (and (empty? list1) (empty? list2))      :equal
      (some #{list1} (partition len1 1 list2)) (if (= len1 len2) :equal :sublist)
      :else                                    :unequal)))

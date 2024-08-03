(ns matching-brackets)

(def ^:private closing
  { \{ \}
    \[ \]
    \( \) })
(def ^:private brackets
  (set (concat (keys closing) (vals closing))))

(defn- update-stack [[top :as stack] c]
  (let [opposite (get closing c)]
    (cond
      opposite  (conj stack opposite)
      (= top c) (pop stack)
      :else     (reduced '(:not-valid)))))

(defn valid? [expression]
  (->> expression
       (filter brackets)
       (reduce update-stack '())
       empty?))

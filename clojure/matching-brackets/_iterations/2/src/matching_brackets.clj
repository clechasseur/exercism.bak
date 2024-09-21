(ns matching-brackets)

(def ^:private closing
  { \{ \}
    \[ \]
    \( \) })
(def ^:private brackets
  (set (concat (keys closing) (vals closing))))

(defn valid? [expression]
  (loop [[c & remain] (filter brackets expression)
         stack '()]
    (if-not c
      (empty? stack)
      (let [opposite (get closing c)]
        (cond
          opposite           (recur remain (conj stack opposite))
          (= (peek stack) c) (recur remain (pop stack))
          :else              false)))))

(ns matching-brackets
  (:require [clojure.string :as string]))

(def ^:private closing
  { \{ \}
    \[ \]
    \( \) })

(defn valid? [expression]
  (let [sanitized (string/replace expression #"[^\[\]{}()]+" "")]
    (loop [[c & remain] sanitized
           stack '()]
      (if-not c
        (empty? stack)
        (let [opposite (get closing c)
              closing? (nil? opposite)
              matches? (= (first stack) c)]
          (cond
            (and closing? (not matches?)) false
            (and closing? matches?)       (recur remain (rest stack))
            :else                         (recur remain (conj stack opposite))))))))

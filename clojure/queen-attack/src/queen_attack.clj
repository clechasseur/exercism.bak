(ns queen-attack
  (:require [clojure.string :as string]))

(defn board-string [{:keys [w b]}]
  (let [board (map (fn [row]
                     (map (fn [col]
                            (condp = [row col]
                              w \W
                              b \B
                              \_))
                          (range 8)))
                   (range 8))]
    (->> board
         (map (partial string/join " "))
         (string/join "\n")
         (#(str % "\n")))))

(defn can-attack [{:keys [w b]}]
  (let [deltas (map (comp #(Math/abs %) -) w b)]
    (or (some zero? deltas) (apply = deltas))))

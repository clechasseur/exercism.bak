(ns minesweeper
  (:require [clojure.string :as string]))

(def ^:private line-separator (System/getProperty "line.separator"))

(def ^:private neighbour-diffs
  [[-1 -1] [0 -1] [1 -1]
   [-1 0]         [1 0]
   [-1 1]  [0 1]  [1 1]])

(defn draw [board-s]
  (let [board (string/split-lines board-s)
        neighbours-count (fn [pt c]
                           (if (= c \*)
                             "*"
                             (->> neighbour-diffs
                                  (map (partial mapv + pt))
                                  (map (fn [[row col]] (get (get board row) col)))
                                  (filter (partial = \*))
                                  count
                                  (#(if (zero? %) " " (String/valueOf %))))))
        counted-board (map-indexed (fn [row-idx row]
                                     (map-indexed (fn [col-idx c]
                                                    (neighbours-count [row-idx col-idx] c))
                                                  row))
                                   board)]
    (string/join line-separator (map string/join counted-board))))

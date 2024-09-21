(ns minesweeper
  (:require [clojure.string :as string]))

(def ^:private line-separator (System/getProperty "line.separator"))

(def ^:private neighbour-diffs
  [[-1 -1] [0 -1] [1 -1]
   [-1 0]         [1 0]
   [-1 1]  [0 1]  [1 1]])

(defn draw [board]
  (let [[first-row :as rows] (string/split-lines board)
        num-rows (count rows)
        num-cols (count first-row)
        at (fn [[row col]] (get (get rows row) col))
        board-map (into {} (for [row (range num-rows)
                                 col (range num-cols)]
                             {[row col] (at [row col])}))
        neighbours-count (fn [pt] (->> neighbour-diffs
                                       (map (partial mapv + pt))
                                       (map board-map)
                                       (filter (partial = \*))
                                       count
                                       String/valueOf
                                       (#(if (= % "0") " " %))))
        count-map (into {} (->> board-map
                                (map (fn [[pt c]]
                                       [pt (if (= c \*) c (neighbours-count pt))]))))
        count-board (map (fn [row] (map (fn [col] (get count-map [row col]))
                                        (range num-cols)))
                         (range num-rows))]
    (string/join line-separator (map string/join count-board))))

(ns minesweeper
  (:require [clojure.string :as string]))

(def ^:private line-separator (System/getProperty "line.separator"))

(def ^:private neighbour-diffs
  [[-1 -1] [0 -1] [1 -1]
   [-1 0]         [1 0]
   [-1 1]  [0 1]  [1 1]])

(defn draw [board]
  (let [board-lines (string/split-lines board)
        indexed-board (map-indexed (fn [row-idx row]
                                     (map-indexed (fn [col-idx c]
                                                    [[row-idx col-idx] c])
                                                  row))
                                   board-lines)
        board-map (into {} (apply concat indexed-board))
        neighbours-count (fn [pt c] (if (= c \*)
                                      "*"
                                      (->> neighbour-diffs
                                           (map (partial mapv + pt))
                                           (map board-map)
                                           (filter (partial = \*))
                                           count
                                           String/valueOf
                                           (#(if (= % "0") " " %)))))
        counted-board (map (fn [indexed-row]
                             (map (fn [[pt c]]
                                    (neighbours-count pt c))
                                  indexed-row))
                           indexed-board)]
    (string/join line-separator (map string/join counted-board))))

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

(defn can-attack [{[wy wx] :w [by bx] :b}]
  (let [xdelta (Math/abs (- wx bx))
        ydelta (Math/abs (- wy by))]
    (or (zero? xdelta) (zero? ydelta) (= xdelta ydelta))))

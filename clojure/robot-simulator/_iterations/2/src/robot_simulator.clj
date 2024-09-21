(ns robot-simulator)

(def ^:private bearings
  {:north {:x 0  :y 1  :left :west  :right :east}
   :east  {:x 1  :y 0  :left :north :right :south}
   :south {:x 0  :y -1 :left :east  :right :west}
   :west  {:x -1 :y 0  :left :south :right :north}})

(defn robot [coordinates bearing]
  {:coordinates coordinates :bearing bearing})

(defn turn-right [bearing]
  (:right (bearing bearings)))

(defn turn-left [bearing]
  (:left (bearing bearings)))

(defn- pt+ [{ax :x ay :y} {bx :x by :y}]
  {:x (+ ax bx) :y (+ ay by)})

(defn- apply-instruction [{:keys [coordinates bearing] :as robot} instruction]
  (case instruction
    \R (update robot :bearing turn-right)
    \L (update robot :bearing turn-left)
    \A (assoc robot :coordinates (pt+ coordinates (bearing bearings)))
    (assert false (str "Invalid instruction: " instruction))))

(defn simulate [instructions robot]
  (reduce apply-instruction robot instructions))

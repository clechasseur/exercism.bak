(ns robot-simulator)

(def ^:private bearings
  {:north {:advance-coord :y :advance-op inc :left :west  :right :east}
   :east  {:advance-coord :x :advance-op inc :left :north :right :south}
   :south {:advance-coord :y :advance-op dec :left :east  :right :west}
   :west  {:advance-coord :x :advance-op dec :left :south :right :north}})

(defn robot [coordinates bearing]
  {:coordinates coordinates :bearing bearing})

(def turn-right (comp :right bearings))
(def turn-left (comp :left bearings))

(defn- advance [bearing coordinates]
  (let [{:keys [advance-coord advance-op]} (bearings bearing)]
    (update coordinates advance-coord advance-op)))

(defn- apply-instruction [robot instruction]
  (case instruction
    \R (update robot :bearing turn-right)
    \L (update robot :bearing turn-left)
    \A (update robot :coordinates (partial advance (:bearing robot)))
    (assert false (str "Invalid instruction: " instruction))))

(defn simulate [instructions robot]
  (reduce apply-instruction robot instructions))

(ns spiral-matrix)

(defn- pt+ [a b]
  (mapv + a b))

(defn- out-of-bounds? [[x y] size]
  (or (< x 0) (>= x size) (< y 0) (>= y size)))

(def ^:private directions
  {:right [1 0]
   :down  [0 1]
   :left  [-1 0]
   :up    [0 -1]})

(defn- next-direction [direction]
  (case direction
    :right :down
    :down  :left
    :left  :up
    :up    :right))

(defn- next-spiral-state [size [pt direction] pts]
  (when (< (count pts) (* size size))
    (let [next-pt (pt+ pt (direction directions))]
      (if-not (or (out-of-bounds? next-pt size) (some #{next-pt} pts))
        [next-pt direction]
        (recur size [pt (next-direction direction)] pts)))))

(defn- spiral-pts [size]
  (loop [[pt direction] [[0 0] :right]
         pts []]
    (if (= (count pts) (* size size))
      pts
      (let [next-pts (conj pts pt)]
        (recur (next-spiral-state size [pt direction] next-pts) next-pts)))))

(defn- spiral-map [size]
  (zipmap (spiral-pts size) (range 1 (inc (* size size)))))

(defn- pts-matrix [size]
  (let [coords (range size)]
    (map
      (fn [y]
        (map (fn [x] [x y]) coords))
      coords)))

(defn spiral [size]
  (let [matrix (pts-matrix size)
        spiral (spiral-map size)]
    (map
      (fn [line]
        (map (fn [pt] (spiral pt)) line))
      matrix)))

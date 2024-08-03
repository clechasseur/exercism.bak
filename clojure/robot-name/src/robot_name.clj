(ns robot-name)

(def ^:private robot-names (atom #{}))

(defn- random-char []
  (char (+ (int \A) (rand-int 26))))

(defn- random-digit []
  (rand-int 10))

(defn- random-robot-name []
  (str (random-char) (random-char) (random-digit) (random-digit) (random-digit)))

(defn- unique-robot-name []
  (loop [name (random-robot-name)]
    (if (not (contains? @robot-names name))
      (do
        (swap! robot-names conj name)
        name)
      (recur (random-robot-name)))))

(defn robot []
  (atom {:name (unique-robot-name)}))

(defn robot-name [robot]
  (:name @robot))

(defn reset-name [robot]
  (swap! robot assoc :name (unique-robot-name)))

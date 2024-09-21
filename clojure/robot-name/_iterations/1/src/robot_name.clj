(ns robot-name)

(defprotocol ^:private Named
  (get-name [this])
  (set-name [this new-name]))

(deftype ^:private Robot [^:unsynchronized-mutable name]
  Named
  (get-name [this] name)
  (set-name [this new-name] (set! name new-name)))

(def ^:private robot-names (atom #{}))

(defn- random-char []
  (char (+ (int \A) (rand-int 26))))

(defn- random-digit []
  (char (+ (int \0) (rand-int 10))))

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
  (Robot. (unique-robot-name)))

(defn robot-name [robot]
  (get-name robot))

(defn reset-name [robot]
  (set-name robot (unique-robot-name)))

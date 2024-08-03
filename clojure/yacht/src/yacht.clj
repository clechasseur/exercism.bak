(ns yacht)

(defn- numbered [number dice]
  (->> dice
       (filter (partial = number))
       count
       (* number)))

(defn- full-house [dice]
  (let [[one two three four five] (sort dice)]
    (cond
      (and (= one two three) (= four five) (not= three four)) (apply + dice)
      (and (= one two) (= three four five) (not= two three)) (apply + dice)
      :else 0)))

(defn- four-of-a-kind [dice]
  (let [[one two three four five] (sort dice)]
    (cond
      (= one two three four) (+ one two three four)
      (= two three four five) (+ two three four five)
      :else 0)))

(defn- straight [starting-dice dice]
  (let [[one _ _ _ five :as sorted-dice] (sort dice)]
    (if (and (= one starting-dice) (= five (+ starting-dice 4)) (apply < sorted-dice)) 30 0)))

(defn- choice [dice]
  (apply + dice))

(defn- yacht [dice]
  (if (apply = dice) 50 0))

(defn score [dice category]
  (case category
    "ones" (numbered 1 dice)
    "twos" (numbered 2 dice)
    "threes" (numbered 3 dice)
    "fours" (numbered 4 dice)
    "fives" (numbered 5 dice)
    "sixes" (numbered 6 dice)
    "full house" (full-house dice)
    "four of a kind" (four-of-a-kind dice)
    "little straight" (straight 1 dice)
    "big straight" (straight 2 dice)
    "choice" (choice dice)
    "yacht" (yacht dice)
    0))

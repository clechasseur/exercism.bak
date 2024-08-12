(ns elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [deck]
  (let [[fir] deck]
    fir))

(defn second-card
  "Returns the second card from deck."
  [deck]
  (let [[_ sec] deck]
    sec))

(defn swap-top-two-cards
  "Returns the deck with first two items reversed."
  [deck]
  (let [[fir sec & rest] deck]
    (apply vector sec fir rest)))

(defn discard-top-card
  "Returns a sequence containing the first card and
   a sequence of the remaining cards in the deck."
  [deck]
  (let [[fir & rest] deck]
    [fir rest]))

(def face-cards
  ["jack" "queen" "king"])

(defn insert-face-cards
  "Returns the deck with face cards between its head and tail."
  [deck]
  (let [[fir & rest] deck]
    (->> [fir face-cards rest]
         flatten
         (remove nil?))))

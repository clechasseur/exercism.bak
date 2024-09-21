(ns poker
  (:require [clojure.string :as string]))

(defn- ->card [value suit]
  {:value value :suit suit})

(defn- card= [& cards]
  (apply = (map :value cards)))

(defn- card-compare [a b]
  (- (:value a) (:value b)))

(def ^:private suits
  {"H" :hearts "D" :diamonds "C" :clubs "S" :spades})

(def ^:private special-cards
  {"J" 11 "Q" 12 "K" 13 "A" 14})

(defn- str->card [s]
  (let [[_ value-s suit-s] (re-matches #"(\d+|[JQKA])([HDCS])" s)
        value (or (special-cards value-s) (Integer/parseInt value-s))
        suit (suits suit-s)]
    (->card value suit)))

(defn- str->cards [s]
  (mapv str->card (string/split s #"\s+")))

(def ^:private hand-kinds
  [:high-card :one-pair :two-pairs :three-of-a-kind :straight :flush
   :full-house :four-of-a-kind :straight-flush :five-of-a-kind])

(defn- compare-cards [cards other-cards]
  (->> (map card-compare cards other-cards)
       (some #(when (not= 0 %) %))
       (#(or % 0))))

(defn- ->hand [kind main-cards other-cards]
  {:kind kind :main-cards main-cards :other-cards other-cards})

(defn- hand-compare [a b]
  (let [kind-cmp (- (.indexOf hand-kinds (:kind a)) (.indexOf hand-kinds (:kind b)))]
    (if (not= 0 kind-cmp)
      kind-cmp
      (let [main-cmp (compare-cards (:main-cards a) (:main-cards b))]
        (if (not= 0 main-cmp)
          main-cmp
          (compare-cards (:other-cards a) (:other-cards b)))))))

(defn- contiguous [n min cards]
  (->> cards
       (partition n 1)
       (filter #(when (apply card= %) %))
       (#(when (>= (count %) min)
           (let [main-cards (apply concat %)
                 main-set (set (map :value main-cards))
                 other-cards (remove (fn [c] (main-set (:value c))) cards)]
             [main-cards other-cards])))))

(defn- straight [[one two three four five :as cards]]
  (let [pairs (partition 2 1 cards)
        descending (fn [coll] (every? (fn [[a b]] (= (card-compare a b) 1)) coll))]
    (or
      (when (descending pairs)
        [cards []])
      ;; Special case: Ace can end a straight
      (when (and
              (= (:value one) (special-cards "A"))
              (= (:value two) 5)
              (descending (rest pairs)))
        [[two three four five one] []]))))

(defn- flush? [cards]
  (apply = (map :suit cards)))

(defn- full-house [[one two three four five]]
  (or
    (when (and (card= one two three) (card= four five))
      [[one two three] [four five]])
    (when (and (card= one two) (card= three four five))
      [[three four five] [one two]])))

(defn- cards->hand [cards]
  (let [sorted-cards (sort #(card-compare %2 %1) cards)
        create-hand (fn [kind [main-cards other-cards]] (->hand kind main-cards other-cards))
        create-main-hand (fn [kind & _] (create-hand kind [sorted-cards []]))]
    (condp #(%1 %2) sorted-cards
      (partial contiguous 5 1)       :>> (partial create-hand :five-of-a-kind)
      #(and (flush? %) (straight %)) :>> (partial create-hand :straight-flush)
      (partial contiguous 4 1)       :>> (partial create-hand :four-of-a-kind)
      full-house                     :>> (partial create-hand :full-house)
      flush?                         :>> (partial create-main-hand :flush)
      straight                       :>> (partial create-hand :straight)
      (partial contiguous 3 1)       :>> (partial create-hand :three-of-a-kind)
      (partial contiguous 2 2)       :>> (partial create-hand :two-pairs)
      (partial contiguous 2 1)       :>> (partial create-hand :one-pair)
      some?                          :>> (partial create-main-hand :high-card))))

(defn best-hands [hands]
  (->> hands
       (map #(vector (str->cards %) %))
       (map (fn [[cards hand-s]] [(cards->hand cards) hand-s]))
       (sort #(hand-compare (first %2) (first %1)))
       ((fn [[[top-hand] :as hands]]
          (->> hands
               (take-while #(zero? (hand-compare (first %) top-hand)))
               (map second))))))

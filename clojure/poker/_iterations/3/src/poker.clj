(ns poker
  (:import (java.lang Comparable))
  (:require [clojure.string :as string]))

(defrecord ^:private Card [value suit]
  Comparable
  (compareTo [_ other]
    (- value (:value other))))

;; We have to define a custom = for Card because we can't override Object::equals in a record
(defn- card= [& cards]
  (apply = (map :value cards)))

(def ^:private suits
  {"H" :hearts "D" :diamonds "C" :clubs "S" :spades})

(def ^:private special-cards
  {"J" 11 "Q" 12 "K" 13 "A" 14})

(defn- str->Card [s]
  (let [[_ value-s suit-s] (re-matches #"(\d+|[JQKA])([HDCS])" s)
        value (or (special-cards value-s) (Integer/parseInt value-s))
        suit (suits suit-s)]
    (->Card value suit)))

(defn- str->cards [s]
  (mapv str->Card (string/split s #"\s+")))

(def ^:private hand-kinds
  [:high-card :one-pair :two-pairs :three-of-a-kind :straight :flush
   :full-house :four-of-a-kind :straight-flush :five-of-a-kind])

(defn- compare-cards [cards other-cards]
  (->> (map compare cards other-cards)
       (some #(when (not= 0 %) %))
       (#(or % 0))))

(defrecord ^:private Hand [kind main-cards other-cards]
  Comparable
  (compareTo [_ other]
    (let [kind-pos (.indexOf hand-kinds kind)
          other-kind-pos (.indexOf hand-kinds (:kind other))
          kind-cmp (- kind-pos other-kind-pos)]
      (if (not= 0 kind-cmp)
        kind-cmp
        (let [main-cmp (compare-cards main-cards (:main-cards other))]
          (if (not= 0 main-cmp)
            main-cmp
            (compare-cards other-cards (:other-cards other))))))))

(defn- contiguous [n min cards]
  (->> cards
       (partition n 1)
       (filter #(when (apply card= %) %))
       (#(when (>= (count %) min)
           (let [main-cards (apply concat %)
                 other-cards (remove (apply sorted-set main-cards) cards)]
             [main-cards other-cards])))))

(defn- straight [[one two three four five :as cards]]
  (let [pairs (partition 2 1 cards)
        descending (fn [coll] (every? (fn [[a b]] (= (compare a b) 1)) coll))]
    (or
      (when (descending pairs)
        [cards []])
      ;; special case: Ace can end a straight
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

(defn- cards->Hand [cards]
  (let [sorted-cards (sort #(compare %2 %1) cards)
        create-Hand (fn [kind [main-cards other-cards]] (->Hand kind main-cards other-cards))
        create-main-Hand (fn [kind & _] (create-Hand kind [sorted-cards []]))]
    (condp #(%1 %2) sorted-cards
      (partial contiguous 5 1)       :>> (partial create-Hand :five-of-a-kind)
      #(and (flush? %) (straight %)) :>> (partial create-Hand :straight-flush)
      (partial contiguous 4 1)       :>> (partial create-Hand :four-of-a-kind)
      full-house                     :>> (partial create-Hand :full-house)
      flush?                         :>> (partial create-main-Hand :flush)
      straight                       :>> (partial create-Hand :straight)
      (partial contiguous 3 1)       :>> (partial create-Hand :three-of-a-kind)
      (partial contiguous 2 2)       :>> (partial create-Hand :two-pairs)
      (partial contiguous 2 1)       :>> (partial create-Hand :one-pair)
      some?                          :>> (partial create-main-Hand :high-card))))

(defn best-hands [hands]
  (->> hands
       (map #(vector (str->cards %) %))
       (map (fn [[cards hand-s]] [(cards->Hand cards) hand-s]))
       (sort #(compare (first %2) (first %1)))
       ((fn [[[top-hand] :as hands]]
          (->> hands
               (take-while #(zero? (compare (first %) top-hand)))
               (map second))))))

(ns poker
  (:require [clojure.string :as string]))

(defrecord ^:private Card [value suit]
  Comparable
  (compareTo [_ other]
    (- value (:value other))))

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

(defn- cards->Hand [cards]
  (let [sorted-cards (sort #(compare %2 %1) cards)]
    ;; TODO
    (->Hand :high-card sorted-cards [])))

(defn best-hands [hands]
  (->> hands
       (map #(vector (str->cards %) %))
       (map (fn [[cards hand-s]] [(cards->Hand cards) hand-s]))
       (sort #(compare (first %2) (first %1)))
       ((fn [[[top-hand] :as hands]]
          (->> hands
               (take-while #(zero? (compare (first %) top-hand)))
               (map second))))))

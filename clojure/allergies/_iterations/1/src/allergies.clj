(ns allergies)

(def ^:private allergens
  [:eggs :peanuts :shellfish :strawberries :tomatoes :chocolate :pollen :cats])

(defn- allergic-to-score? [score allergen-score]
  (pos? (bit-and score (bit-shift-left 1 allergen-score))))

(defn allergies [score]
  (->> (range (count allergens))
       (filter (partial allergic-to-score? score))
       (map (partial get allergens))))

(defn allergic-to? [score allergen]
  (allergic-to-score? score (.indexOf allergens allergen)))

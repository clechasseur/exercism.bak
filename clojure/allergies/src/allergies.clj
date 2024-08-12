(ns allergies)

(def ^:private allergens
  [:eggs :peanuts :shellfish :strawberries :tomatoes :chocolate :pollen :cats])

(defn allergies [score]
  (->> (range (count allergens))
       (filter (partial bit-test score))
       (map (partial get allergens))))

(defn allergic-to? [score allergen]
  (bit-test score (.indexOf allergens allergen)))

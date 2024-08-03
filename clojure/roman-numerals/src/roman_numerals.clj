(ns roman-numerals)

(def ^:private roman-number-infos
  [{:one "I" :five "V" :ten "X"}
   {:one "X" :five "L" :ten "C"}
   {:one "C" :five "D" :ten "M"}
   {:one "M"}])

(defn- digit-to-roman [digit infos]
  (cond
    (< digit 4) (apply str (repeat digit (:one infos)))
    (= digit 4) (str (:one infos) (:five infos))
    (< digit 9) (str (:five infos) (apply str (repeat (- digit 5) (:one infos))))
    (= digit 9) (str (:one infos) (:ten infos))))

(defn numerals [num]
  (loop [remaining num
         infos-idx 0
         roman '()]
    (if (zero? remaining)
      (apply str roman)
      (let [digit (mod remaining 10)
            infos (get roman-number-infos infos-idx)
            roman-part (digit-to-roman digit infos)]
        (recur (quot remaining 10) (inc infos-idx) (conj roman roman-part))))))

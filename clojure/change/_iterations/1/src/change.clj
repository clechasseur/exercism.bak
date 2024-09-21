(ns change)

(defn- possibly-issue
  ([amount coins] (possibly-issue amount coins Integer/MAX_VALUE))
  ([amount coins steps-left]
   (cond
     (zero? amount)     []
     (zero? steps-left) nil
     :else (reduce
             (fn [change coin]
               (let [sub-change (possibly-issue
                                  (- amount coin)
                                  (drop-while (partial < coin) coins)
                                  (min
                                    (dec steps-left)
                                    (dec (if (nil? change) Integer/MAX_VALUE (count change)))))]
                 (if (and
                       (some? sub-change)
                       (or
                         (nil? change)
                         (< (inc (count sub-change)) (count change))))
                   (conj sub-change coin)
                   change)))
             nil
             (drop-while (partial < amount) coins)))))

(defn issue [amount coins]
  (let [rcoins (reverse (sort coins))
        rcoins-except-one (if (= 1 (last coins)) (drop-last 1 coins) nil)
        change-without-one (if (some? rcoins-except-one) (possibly-issue amount rcoins-except-one) nil)
        change (if (nil? change-without-one) (possibly-issue amount rcoins) change-without-one)]
    (when (nil? change)
      (throw (IllegalArgumentException. "cannot change")))
    change))

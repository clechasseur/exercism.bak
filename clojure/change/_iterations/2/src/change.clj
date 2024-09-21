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
  (let [rcoins (sort > coins)
        rcoins-except-one (when (= 1 (last coins)) (drop-last 1 coins))
        change-without-one (when rcoins-except-one (possibly-issue amount rcoins-except-one))
        change (or change-without-one (possibly-issue amount rcoins))]
    (when-not change
      (throw (IllegalArgumentException. "cannot change")))
    change))

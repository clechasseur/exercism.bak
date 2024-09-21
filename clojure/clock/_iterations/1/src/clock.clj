(ns clock)

(defn clock->string [[hours minutes]]
  (format "%02d:%02d" hours minutes))

(defn clock [hours minutes]
  (let [hours (mod hours 24)]
    (cond
      (<= 0 minutes 59) [hours minutes]
      (>= minutes 60)   (recur (inc hours) (- minutes 60))
      (neg? minutes)    (recur (dec hours) (+ minutes 60)))))

(defn add-time [[hours minutes] minutes-diff]
  (clock hours (+ minutes minutes-diff)))

(ns clock)

(def minutes-per-hour 60)
(def minutes-per-day (* 24 minutes-per-hour))

(defn clock->string [c]
  (format "%02d:%02d" (quot c minutes-per-hour) (rem c minutes-per-hour)))

(defn clock [hours minutes]
  (mod (+ (* hours minutes-per-hour) minutes) minutes-per-day))

(defn add-time [c minutes-diff]
  (clock 0 (+ c minutes-diff)))

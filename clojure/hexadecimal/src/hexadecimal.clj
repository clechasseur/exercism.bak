(ns hexadecimal)

(def ^:private hex-digits (sorted-set \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f))
(def ^:private hex-digit-to-int (zipmap hex-digits (range 16)))

(defn hex-to-int [hex]
  (if-not (every? hex-digits hex)
    0
    (->> hex
         (map hex-digit-to-int)
         (reduce #(+ %2 (* %1 16))))))

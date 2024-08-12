(ns octal)

(defn to-decimal [octal]
  (if-not (re-matches #"[0-7]+" octal)
    0
    (reduce
      (fn [total digit]
        (+ (* total 8) (Character/digit digit 8)))
      0
      octal)))

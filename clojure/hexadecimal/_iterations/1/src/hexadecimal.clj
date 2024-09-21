(ns hexadecimal)

(defn- hex-digit-to-int [xdigit]
  (let [xint (int xdigit)]
    (cond
      (<= (int \0) xint (int \9)) (- xint (int \0))
      (<= (int \a) xint (int \f)) (+ (- xint (int \a)) 10))))

(defn hex-to-int [hex]
  (if-not (re-matches #"[0-9a-f]*" hex)
    0
    (loop [[xdigit & xrest] hex
           dec 0]
      (if-not xdigit
        dec
        (recur xrest (+ (hex-digit-to-int xdigit) (* dec 16)))))))

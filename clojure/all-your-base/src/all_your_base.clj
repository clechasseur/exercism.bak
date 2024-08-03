(ns all-your-base)

(defn- valid-base? [base]
  (> base 1))

(defn- valid-number? [base num]
  (and (valid-base? base) (number? num)))

(defn- valid-digits? [base digits]
  (and (valid-base? base) (seq digits) (every? #(<= 0 % (dec base)) digits)))

(defn to-dec [base digits]
  (when (valid-digits? base digits)
    (reduce #(+ (* base %1) %2) 0 digits)))

(defn from-dec [base num]
  (when (valid-number? base num)
    (loop [quotient num
           digits '()]
      (if (zero? quotient)
        (or (seq digits) '(0))
        (recur (quot quotient base) (conj digits (mod quotient base)))))))

(defn convert [from-base digits to-base]
  (->> digits
       (to-dec from-base)
       (from-dec to-base)))

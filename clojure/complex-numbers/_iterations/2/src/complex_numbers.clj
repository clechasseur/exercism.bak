(ns complex-numbers)

(defn- square [n]
  (* n n))

(defn real [[a]]
  a)

(defn imaginary [[_ b]]
  b)

(defn abs [[a b]]
  (Math/sqrt (+ (square a) (square b))))

(defn conjugate [[a b]]
  [a (- b)])

(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn sub [[a b] [c d]]
  [(- a c) (- b d)])

(defn mul [[a b] [c d]]
  [(- (* a c) (* b d))
   (+ (* b c) (* a d))])

(defn div [[a b] [c d]]
  (let [divisor (double (+ (square c) (square d)))]
    [(/ (+ (* a c) (* b d)) divisor)
     (/ (- (* b c) (* a d)) divisor)]))

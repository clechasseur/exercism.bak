(ns diamond)

(defn- whitespace [n]
  (apply str (repeat n " ")))

(defn- limit-row [last-letter]
  (let [external-whitespace (whitespace (- (int last-letter) (int \A)))]
    (str external-whitespace "A" external-whitespace)))

(defn- row [last-letter letter]
  (if (= letter \A)
    (limit-row last-letter)
    (let [external-whitespace (whitespace (- (int last-letter) (int letter)))
          internal-whitespace (whitespace (dec (* 2 (- (int letter) (int \A)))))]
      (str external-whitespace letter internal-whitespace letter external-whitespace))))

(defn- rows [last-letter]
  (let [letters (map char (range (int \A) (inc (int last-letter))))]
    (map (partial row last-letter) letters)))

(defn diamond [last-letter]
  (let [forward-rows (rows last-letter)
        backward-rows (reverse (drop-last forward-rows))]
    (concat forward-rows backward-rows)))

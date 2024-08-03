(ns binary-search-tree)

(defn singleton [data]
  {:data data})

(def value :data)
(def left :left)
(def right :right)

(defn insert [data node]
  (if (nil? node)
    (singleton data)
    (let [direction (if (<= data (value node)) left right)]
      (assoc node direction (insert data (direction node))))))

(defn to-list [node]
  (if (nil? node)
    '()
    (concat (to-list (left node)) (list (value node)) (to-list (right node)))))

(defn from-list [coll]
  (reduce #(insert %2 %1) nil coll))

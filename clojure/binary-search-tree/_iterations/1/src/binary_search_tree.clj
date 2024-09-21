(ns binary-search-tree)

(defn singleton [data]
  (atom {:data data :left nil :right nil}))

(defn value [node]
  (:data @node))

(defn- insert-node! [new-node in-node]
  (let [key (if (<= (value new-node) (value in-node)) :left :right)
        sub-node (key @in-node)]
    (if (nil? sub-node)
      (swap! in-node assoc key new-node)
      (insert-node! new-node sub-node))))

(defn insert [data node]
  (insert-node! (singleton data) node)
  node)

(defn left [node]
  (:left @node))

(defn right [node]
  (:right @node))

(defn to-list [node]
  (if (nil? node)
    '()
    (concat (to-list (left node)) (list (value node)) (to-list (right node)))))

(defn from-list [[first & to-insert]]
  (loop [node (singleton first)
         [data & rest :as all] to-insert]
    (if (empty? all)
      node
      (recur (insert data node) rest))))

(ns dominoes)

(defn- remove-first [x coll]
  (let [[before [_ & after]] (split-with (partial not= x) coll)]
    (concat before after)))

(defn- reverse-domino [domino]
  (vec (rseq domino)))

(defn- domino-combinations [domino]
  (if (= (first domino) (last domino))
    [domino]
    [domino (reverse-domino domino)]))

(defn- chain-head [chain]
  (first (first chain)))

(defn- chain-tail [chain]
  (last (last chain)))

(defn- valid-chain? [chain]
  (= (chain-head chain) (chain-tail chain)))

(defn- chains
  ([dominoes] (chains dominoes []))
  ([dominoes chain]
   (if (empty? dominoes)
     [chain]
     (mapcat
       (fn [domino]
         (cond
           (empty? chain)                        (mapcat
                                                   #(chains (remove-first domino dominoes) [%])
                                                   (domino-combinations domino))
           (= (first domino) (chain-tail chain)) (chains (remove-first domino dominoes) (conj chain domino))
           (= (last domino) (chain-tail chain))  (chains (remove-first domino dominoes) (conj chain (reverse-domino domino)))
           :else                                 []))
       dominoes))))

(defn can-chain? [dominoes]
  (or
    (empty? dominoes)
    (some valid-chain? (chains dominoes))))

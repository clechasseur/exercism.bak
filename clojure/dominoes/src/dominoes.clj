(ns dominoes)

(defn- remove-first [x coll]
  (let [[before [_ & after]] (split-with (partial not= x) coll)]
    (concat before after)))

(defn- reverse-domino [domino]
  (vec (rseq domino)))

(defn- chain-head [[[head]]]
  head)

(defn- chain-tail [chain]
  (last (last chain)))

(defn- valid-chain? [chain]
  (= (chain-head chain) (chain-tail chain)))

(defn- chains [dominoes chain]
 (if (empty? dominoes)
   [chain]
   (mapcat
     (fn [domino]
       (let [tail (chain-tail chain)
             remaining-dominoes (remove-first domino dominoes)]
         (cond
           (= (first domino) tail) (chains remaining-dominoes (conj chain domino))
           (= (last domino) tail)  (chains remaining-dominoes (conj chain (reverse-domino domino)))
           :else                   [])))
     dominoes)))

(defn can-chain? [[first-domino & other-dominoes]]
  (or
    (nil? first-domino)
    (some valid-chain? (chains other-dominoes [first-domino]))))

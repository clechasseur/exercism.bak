(ns pov)

(defn- conj-if-not-empty [coll x]
  (cond-> coll
          (seq x) (conj x)))

(defn of
  ([new-root tree]
   (of new-root tree []))
  ([new-root [value & children :as tree] above]
   (if (= new-root value)
     (conj-if-not-empty tree above)
     (some
       (fn [child]
         (let [other-children (remove (partial identical? child) children)
               above-child (-> (apply vector value other-children)
                               (conj-if-not-empty above))]
           (of new-root child above-child)))
       children))))

(defn- path-to
  ([to tree]
   (path-to to tree []))
  ([to [root & children] above]
   (let [path (conj above root)]
     (if (= root to)
       path
       (some #(path-to to % path) children)))))

(defn path-from-to [from to tree]
  (when-let [of-from (of from tree)]
    (path-to to of-from)))

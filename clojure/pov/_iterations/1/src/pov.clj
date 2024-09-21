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
     (reduce
       (fn [_ child]
         (let [other-children (filter (partial not= child) children)
               above-child (-> (apply vector value other-children)
                               (conj-if-not-empty above))
               of-child (of new-root child above-child)]
           (when of-child
             (reduced of-child))))
       nil
       children))))

(defn- path-to
  ([to tree]
   (path-to to tree []))
  ([to [root & children] above]
   (let [path (conj above root)]
     (if (= root to)
       path
       (reduce
         (fn [_ child]
           (let [sub-path (path-to to child path)]
             (when sub-path
               (reduced sub-path))))
         nil
         children)))))

(defn path-from-to [from to tree]
  (let [of-from (of from tree)]
    (when of-from
      (path-to to of-from))))

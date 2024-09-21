(ns kindergarten-garden
  (:require [clojure.string :as string]))

(def ^:private plants
  {\G :grass \C :clover \R :radishes \V :violets})

(def ^:private default-students
  ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny" "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])

(defn garden
  ([diagram]
   (garden diagram default-students))
  ([diagram students]
   (let [student-symbols (map #(keyword (string/lower-case %)) (sort students))]
     (->> (string/split-lines diagram)
          (map (partial partition 2))
          (apply map concat)
          (map (partial mapv plants))
          (zipmap student-symbols)))))

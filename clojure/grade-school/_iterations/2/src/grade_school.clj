(ns grade-school)

(defn grade
  "Returns the students registered in the school roster in the given grade."
  [school grade]
  (school grade []))

(defn add
  "Adds a student to the school roster in the given grade."
  [school name grade]
  (merge-with into school {grade [name]}))

(defn sorted
  "Returns a list of all grades in the school roster.
   Grades are sorted in ascending order, and students are sorted alphabetically in a grade."
  [school]
  (->> school
       (mapv (fn [[grade names]] [grade (sort names)]))
       (into (sorted-map))))

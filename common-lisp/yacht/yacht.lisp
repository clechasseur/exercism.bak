(defpackage :yacht
  (:use :cl)
  (:export :score))

(in-package :yacht)

;; This is pretty much a straight port of my Clojure solution:
;; https://exercism.org/tracks/clojure/exercises/yacht/solutions/clechasseur

(defun numbered (number dice)
  (let ((matching-dice (remove-if (lambda (n) (/= n number)) dice)))
       (* (list-length matching-dice) number)))

(defun full-house (dice)
  (destructuring-bind (one two three four five) (sort (copy-list dice) #'<)
    (cond
      ((and (= one two three) (= four five) (/= three four)) (apply #'+ dice))
      ((and (= one two) (= three four five) (/= two three)) (apply #'+ dice))
      (t 0))))

(defun four-of-a-kind (dice)
  (destructuring-bind (one two three four five) (sort dice #'<)
    (cond
      ((= one two three four) (+ one two three four))
      ((= two three four five) (+ two three four five))
      (t 0))))

(defun straight (starting-dice dice)
  (let ((sorted-dice (sort dice #'<)))
       (destructuring-bind (one _ _ _ five) sorted-dice
         (if (and (= one starting-dice) (= five (+ starting-dice 4)) (apply #'< sorted-dice))
             30
             0))))

(defun choice (dice)
  (apply #'+ dice))

(defun yacht (dice)
  (if (apply #'= dice)
      50
      0))

(defun score (dice category)
  "Returns the score of the dice for the given category."
  (case category
    (:ones (numbered 1 dice))
    (:twos (numbered 2 dice))
    (:threes (numbered 3 dice))
    (:fours (numbered 4 dice))
    (:fives (numbered 5 dice))
    (:sixes (numbered 6 dice))
    (:full-house (full-house dice))
    (:four-of-a-kind (four-of-a-kind dice))
    (:little-straight (straight 1 dice))
    (:big-straight (straight 2 dice))
    (:choice (choice dice))
    (:yacht (yacht dice))
    (otherwise 0)))

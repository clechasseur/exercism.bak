(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

(defmacro flour (expr)
  ;; A little pizza joke, haha funny right?
  `(floor ,expr)
  )

(defun dough-calculator (pizzas diameter)
  "Calculate the amount of dough required to make the specified number of pizzas of the specified diameter"
  ;; Formula: g = n * (((45 * pi * d) / 20) + 200)
  (round (* pizzas (+ (/ (* 45 pi diameter) 20) 200))))

(defun size-from-sauce (sauce)
  "Calculate the diameter of a pizza that uses the specified amount of sauce"
  ;; Formula: d = square-root of ((40 * s) / (3 * pi))
  (sqrt (/ (* 40 sauce) (* 3 pi))))

(defun pizzas-per-cube (cube-size diameter)
  "Calculate the number of pizzas of the specified diameter one can make with a block of cheese of the given side-length"
  ;; Formula: n = (2 * (l^3)) / (3 * pi * (d^2))
  (flour (/ (* 2 (expt cube-size 3)) (* 3 pi (expt diameter 2)))))

(defun fair-share-p (pizzas friends)
  "Determine if the given number of pizzas (with 8 slices each) can be evenly divided among the given number of friends"
  (zerop (mod (* pizzas 8) friends)))

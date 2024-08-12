(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven ()
  "Return the expected time the lasagna will spend in the oven"
  337)

(defun remaining-minutes-in-oven (spent-so-far)
  "Return the remaining time the lasagna will spend in the over based on the time spent so far"
  (- (expected-time-in-oven) spent-so-far))

(defun preparation-time-in-minutes (layers)
  "Return the preparation time for a lasagna, each layer taking 19 minutes to prepare"
  (* layers 19))

(defun elapsed-time-in-minutes (layers spent-so-far-in-oven)
  "Determine the elapsed time since starting work on the lasagna based on the number of layers prepared and the time spent so far in the oven"
  (+ (preparation-time-in-minutes layers) spent-so-far-in-oven))

(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((pling (when (zerop (mod n 3)) "Pling"))
        (plang (when (zerop (mod n 5)) "Plang"))
        (plong (when (zerop (mod n 7)) "Plong")))
    (let ((plingplangplong (concatenate 'string pling plang plong)))
      (if (string= "" plingplangplong)
        (write-to-string n)
        plingplangplong))))

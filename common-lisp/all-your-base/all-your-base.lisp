(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

; returns the list (n-1 n-2 ... 0)
(defun n-1-to-0 (n)
  (labels ((rec (num acc)
         (if (>= num n)
         acc
         (rec (1+ num) (cons num acc)))))
    (rec 0 nil)))

(defun rebase (list-digits in-base out-base)
  (let ((from-base (reduce #'+ (mapcar #'* list-digits
                       (mapcar (lambda (num) (expt in-base num))
                           (n-1-to-0 (length list-digits)))))))
    (labels ((rec (num out-base acc)
           (if (zerop num)
           acc
           (multiple-value-bind (quoty remy) (floor num out-base)
             (rec quoty out-base (cons remy acc))))))
      (rec from-base out-base nil))))
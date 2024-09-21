(defpackage :leslies-lists
  (:use :cl)
  (:export :new-list
           :list-of-things
           :add-to-list
           :first-thing
           :second-thing
           :third-thing
           :twenty-third-thing
           :remove-first-item
           :on-the-list-p
           :list-append
           :just-how-long
           :part-of-list
           :list-reverse))

(in-package :leslies-lists)

(defun new-list ()
  "Create a new empty list"
  '())

(defun list-of-things (thing1 thing2 thing3)
  "Create a list of three things"
  (list thing1 thing2 thing3))

(defun add-to-list (item list)
  "Add an item to an existing list (at the beginning)"
  (append (list item) list)) ; Not sure how that works - how does the compiler find the list function when it's shadowed by the list variable? :D

(defun first-thing (list)
  "Return the first thing in the list"
  (first list))

(defun second-thing (list)
  "Return the second thing in the list"
  (second list))

(defun third-thing (list)
  "Return the third thing in the list"
  (third list))

(defun twenty-third-thing (list)
  "Return the 23rd thing in the list (how cool is that)"
  (nth 22 list)) ; Because lists are 0-based

(defun remove-first-item (list)
  "Remove the first item in the list and return the remaining items"
  (rest list))

(defun list-append (list1 list2)
  "Concatenate two lists"
  (append list1 list2))

(defun just-how-long (list)
  "Return the number of items in the list"
  (length list))

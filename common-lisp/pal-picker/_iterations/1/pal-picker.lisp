(defpackage :pal-picker
  (:use :cl)
  (:export :pal-picker :habitat-fitter :feeding-time-p
           :pet :play-fetch))

(in-package :pal-picker)

(defun pal-picker (personality)
  "Pick a proper pet for your personality"
  (case personality
    (:lazy      "Cat")
    (:energetic "Dog")
    (:quiet     "Fish")
    (:hungry    "Rabbit")
    (:talkative "Bird")
    (otherwise  "I don't know... A dragon?")))

(defun habitat-fitter (weight)
  "Find the right habitat for a pet of the given weight"
  (cond
    ((>= weight 40) :massive)
    ((>= weight 20) :large)
    ((>= weight 10) :medium)
    ((plusp weight) :small)
    (t              :just-your-imagination)))

(defun feeding-time-p (fullness)
  "Check if it's time to feed your pet"
  (if (> fullness 20)
      "All is well."
      "It's feeding time!"))

(defun pet (pet)
  "Check if it's possible to pet this type of pet"
  (when (string= pet "Fish")
    "Maybe not with this pet..."))

(defun play-fetch (pet)
  "Check if it's possible to play fetch with this type of pet"
  (unless (string= pet "Dog")
    "Maybe not with this pet..."))

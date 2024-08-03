(ns meetup
  (:require [clojure.string :as string])
  (:import (java.time DayOfWeek LocalDate)))

(def ^:private numd-descriptors
  (zipmap [:first :second :third :fourth :fifth] (range)))

(defn- first-day-of-month [month year]
  (LocalDate/of ^Integer year ^Integer month 1))

(defn- to-day-of-week [day]
  (DayOfWeek/valueOf (string/upper-case (name day))))

(defn- move-to-day [date day]
  (if (= (.getDayOfWeek date) (to-day-of-week day))
    date
    (recur (.plusDays date 1) day)))

(defn- nth-day [month year day descriptor]
  (let [loop-count (descriptor numd-descriptors)]
    (loop [date (move-to-day (first-day-of-month month year) day)
           i loop-count]
      (if (zero? i)
        date
        (recur (.plusDays date 7) (dec i))))))

(defn- last-day [month year day]
  (loop [date (move-to-day (first-day-of-month month year) day)]
    (let [next-date (.plusDays date 7)]
      (if (not= (.getMonthValue date) (.getMonthValue next-date))
        date
        (recur next-date)))))

(defn- teenth-day [month year day]
  (loop [date (move-to-day (first-day-of-month month year) day)]
    (if (<= 13 (.getDayOfMonth date) 19)
      date
      (recur (.plusDays date 7)))))

(defn- meetup-date [month year day descriptor]
  (condp contains? descriptor
    numd-descriptors (nth-day month year day descriptor)
    #{:last}         (last-day month year day)
    #{:teenth}       (teenth-day month year day)
    (throw (IllegalArgumentException. "Invalid descriptor"))))

(defn meetup [month year day descriptor]
  (let [date (meetup-date month year day descriptor)]
    [(.getYear date) (.getMonthValue date) (.getDayOfMonth date)]))

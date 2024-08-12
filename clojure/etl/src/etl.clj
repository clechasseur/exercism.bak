(ns etl
  (:require [clojure.string :as string]))

(defn transform [src]
  (into {} (for [[score keys] src
                 key keys]
             [(string/lower-case key) score])))

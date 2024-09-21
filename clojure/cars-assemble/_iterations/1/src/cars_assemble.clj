(ns cars-assemble)

(def base-production 221.0)

(defn success-rate
  "Returns the assembly line's success rate at the given speed"
  [speed]
  (condp some (list speed)
    #{0} 0.0
    #{1 2 3 4} 1.0
    #{5 6 7 8} 0.9
    #{9} 0.8
    #{10} 0.77))

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (* base-production speed (success-rate speed)))

(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (int (Math/floor (/ (production-rate speed) 60))))

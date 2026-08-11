;; TODO: Rework this so it's abstracted away from Polynomial Interpolation

(ns physics.kinematics
  (:require  
    [physics.poly-int :as poly-int] ; Polynomial Interpolation
    [physics.vector3  :as v3]))

(defn jerk-profile
  "Calculates the Jerk Profile using Polynomial Interpolation.
  The constraints determine the order of the polynomial."
  [N constraints] (poly-int/jerk-profile N constraints))

(defn accel-profile
  "Calculates the Acceleration Profile using Polynomial Interpolation.
  The constraints determine the order of the polynomial."
  [N constraints] (poly-int/accel-profile N constraints))

(defn velocity-profile
  "Calculates the Velocity Profile using Polynomial Interpolation.
  The constraints determine the order of the polynomial."
  [N constraints] (poly-int/velocity-profile N constraints))

(defn jerk-crit-time 
  "Calculates the critical points of the Jerk Profile."
  [N constraints] (poly-int/jerk-crit-time N constraints))

(defn accel-crit-time 
  "Calculates the critical points of the Acceleration Profile"
  [N constraints] (poly-int/accel-crit-time N constraints))

(defn jerk-peak
  "Calculates the peak the Jerk Profile"
  [N constraints] (poly-int/jerk-peak N constraints))
(defn accel-peak
  "Calculates the peak of the Acceleration Profile"
  [N constraints] (poly-int/accel-peak N constraints))
(defn velocity-peak
  "Calculates the peak of the Velocity Profile"
  [N constraints] (poly-int/velocity-peak N constraints))

(defrecord StateVector [jrk acc vel])

(defn peak-values [N constr]
  (->StateVector
    (jerk-peak N constr)
    (accel-peak N constr)
    (velocity-peak N constr)))

(defn midpoint [a b] (/ (+ a b) 2))

(defn valid-time?
  [{max-jrk :jrk  max-acc :acc max-vel :vel}
   {peak-jrk :jrk peak-acc :acc  peak-vel :vel}]
  (and 
    (v3/lt peak-jrk max-jrk)
    (v3/lt peak-acc max-acc)
    (v3/lt peak-vel max-vel)))

; TODO: this needs rewritten
(defn search-target-time
  ([max-vals constraints] (search-target-time max-vals 0.01 10 0.1 constraints))
  ([max-vals t-min t-max tol constr]
  (let [t-mid     (midpoint t-min t-max)
        peak-vals (peak-values t-mid constr)]
    (cond
      (< (- t-max t-min) tol) t-mid
      (valid-time? max-vals peak-vals) (search-target-time max-vals t-min t-mid tol constr)
      :else (search-target-time max-vals t-mid t-max tol constr)))))


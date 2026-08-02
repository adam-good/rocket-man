(load-file "vector3.clj")
(load-file "poly_int.clj")

(ns physics.time-profile
  (:require
   [vector3 :as v3] 
   [physics.poly-int :as poly]))

;; TODO this gotta go elsewhere
(defrecord StateVector [jrk acc vel])

(defn peak-values [N constr]
  (->StateVector
    (poly/jerk-peak N constr)
    (poly/accel-peak N constr)
    (poly/velocity-peak N constr)))

(defn n-mid [a b] (/ (+ a b) 2))

(defn valid-time?
  [{max-jrk :jrk  max-acc :acc max-vel :vel}
   {peak-jrk :jrk peak-acc :acc  peak-vel :vel}]
  (and 
    (v3/lt peak-jrk max-jrk)
    (v3/lt peak-acc max-acc)
    (v3/lt peak-vel max-vel)))

;; TODO: this needs rewritten
(defn search-target-time
  [max-vals t-min t-max tol constr]
  (let [t-mid     (n-mid t-min t-max)
        peak-vals (peak-values t-mid constr)]
    (cond
      (< (- t-max t-min) tol) t-mid
      (valid-time? max-vals peak-vals) (search-target-time max-vals t-min t-mid tol constr)
      :else (search-target-time max-vals t-mid t-max tol constr))))

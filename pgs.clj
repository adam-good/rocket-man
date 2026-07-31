(load-file "vector3.clj")
(load-file "physics.clj")
(load-file "utils.clj")
(load-file "json.clj")

(ns pgs
  (:require
   [vector3 :as v3]
   [physics :as phy]
   [utils   :as utl]
   [json    :as json])
  (:require
   [utils :refer [unimplemented]]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 3e-2)))
(defn zero-does-not-exist [n] (if (== n 0) 1e-20 n))

(defn peak-values [N constraints]
  {:peak-jerk (jerk-peak  N constraints)
   :peak-acc  (accel-peak N constraints)
   :peak-vel  (vel-peak   N constraints)})

(defrecord max-values [max-jerk max-accel max-vel])
(defn n-mid [a b] (/ (+ a b) 2))
(defn t-is-valid? [{max-jerk  :max-jerk  max-accel :max-accel max-vel  :max-vel}
                   {peak-jerk :peak-jerk peak-accel  :peak-acc  peak-vel :peak-vel}]
  (and
   (v3/lt peak-jerk max-jerk)
   (v3/lt peak-accel max-accel)
   (v3/lt peak-vel max-vel)))
(defn search-target-time
  [{max-jerk :max-jerk max-accel :max-accel max-vel :max-vel} ;; max values
   t-min t-max tol
   constraints] ; constraints
  (let [t-mid (n-mid t-min t-max)
        peak-vals   (peak-values t-mid constraints)
        max-vals    (->max-values max-jerk max-accel max-vel)]
    (cond
      (< (- t-max t-min) tol) t-mid
      (t-is-valid? max-vals peak-vals) (search-target-time max-vals t-min t-mid tol constraints)
      :else (search-target-time max-vals t-mid t-max tol constraints))))

(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [N pos vel acc targ]
  (jerk-profile N {:r0 pos :v0 vel :a0 acc :rt targ}))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
(def projectile
  (phy/->PhysicalObj
   (v3/zero)               ; Position 
   (v3/->Vector3 0 0 0.2)  ; Velocity 
   (v3/zero)               ; Acceleration 
   1))
(def dt 0.01)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate
   #(phy/update-obj %
                    (guidance-system
                     (search-target-time
                      {:max-jerk (v3/const 8) :max-accel (v3/const 16) :max-vel (v3/const 32)}
                      0.01 10 0.1
                      {:r0 (:position %) :v0 (:velocity %) :a0 (:acceleration %) :rt target})
                     (:position %) (:velocity %) (:acceleration %) target) dt)
   projectile))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (for [[timestep, data] (utl/zip time-series result)]
    {:timestep timestep :datapoint data}))
(def dataset (take 600 raw-data))

(require '[clojure.java.io :as io])
(def json-data (json/write-str dataset))
(with-open [file (io/writer "./output/test.json")]
  (.write file json-data))




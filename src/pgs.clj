(ns pgs
  (:require
   [physics.vector3 :as v3]
   [physics.state   :as state]
   [utils.utils     :as utils]
   [utils.json      :as json]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 3e-2)))
;; TODO: Make sure we don't need this BS
;; (defn zero-does-not-exist [n] (if (== n 0) 1e-20 n))


(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [N pos vel acc targ]
  (jerk-profile N {:r0 pos :v0 vel :a0 acc :rt targ}))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
(def projectile
  (state/->PhysicalObj
   (v3/zero)               ; Position 
   (v3/->Vector3 0 0 0.2)  ; Velocity 
   (v3/zero)               ; Acceleration 
   1))
(def dt 0.01)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate
   #(state/update-obj %
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
  (for [[timestep, data] (utils/zip time-series result)]
    {:timestep timestep :datapoint data}))
(def dataset (take 600 raw-data))

(require '[clojure.java.io :as io])
(def json-data (json/write-str dataset))
(with-open [file (io/writer "./output/test.json")]
  (.write file json-data))




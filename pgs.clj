(load-file "vector3.clj")
(load-file "physics.clj")
(load-file "utils.clj")
(load-file "json.clj")

(ns pgs
  (:require
   [vector3 :as v3]
   [physics :as phi]
   [utils   :as utl]
   [json    :as json]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 7e-2)))

(defn target-acceleration [pos vel acc targ-pos k_p k_d]
  (let [dir      (v3/elem-subtract targ-pos pos)
        dist     (distance pos targ-pos)
        targ-speed (-> (distance vel targ-pos) (/ dist dist dist))
        targ-vel (v3/scalar-product  targ-speed dir)
        err-vel  (v3/elem-subtract targ-vel vel)]

    (v3/elem-add
     acc
     (v3/scalar-product k_p err-vel)
     (v3/scalar-product (* -1 k_d) vel))))

(defn jerk [target-accel accel]
  (v3/elem-subtract target-accel accel))

(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [pos vel acc targ k_p k_d]
  (jerk (target-acceleration pos vel acc targ k_p k_d) acc))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
(def projectile
  (phi/->PhysicalObj
   (v3/zero)               ; Position 
   (v3/->Vector3 0 0 0.2)  ; Velocity 
   (v3/zero)               ; Acceleration 
   1))
(def dt 0.05)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate
   #(phi/update-obj %
                    (guidance-system
                     (:position %) (:velocity %) (:acceleration %) target
                     0.5 0.99) dt)
   projectile))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (for [[timestep, data] (utl/zip time-series result)]
    {:timestep timestep :datapoint data}))
(def dataset (take 300 raw-data))

(require '[clojure.java.io :as io])
(def json-data (json/write-str dataset))
(with-open [file (io/writer "./output/test.json")]
  (.write file json-data))




(load-file "vector3.clj")
(load-file "physics.clj")
(load-file "utils.clj")

(ns pgs
  (:require
   [vector3 :as v3]
   [physics :as phi]
   [utils   :as utl]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 1e-5)))

(defn calculate-pos-error [position target] (v3/elem-subtract target position))
(defn calculate-target-accel [pos-err dampening] 
  (v3/scalar-product dampening pos-err))
(defn calculate-jerk [target-accel accel dampening] 
  (v3/scalar-product dampening (v3/elem-subtract target-accel accel)))

(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [pos acc targ alpha beta]
  (-> 
   (calculate-pos-error pos targ) 
   (calculate-target-accel alpha) 
   (calculate-jerk acc beta) ))

;; Initial Conditions
(def target (v3/->Vector3 2 2 2))
(def projectile
  (phi/->PhysicalObj
   (v3/zero)             ; Position 
   (v3/->Vector3 0 0 1)  ; Velocity 
   (v3/zero)             ; Acceleration 
   1))
(def dt 0.05)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate 
   #(phi/update-obj % 
      (guidance-system 
       (:position %) (:acceleration %) target
       0.1 0.5) dt) 
   projectile ))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (utl/zip time-series (for [obj result] obj)))

;; Write to CSV
(require '[clojure.java.io :as io] '[clojure.string :refer [join]])
(def csv-data
  (for [datapoint raw-data] (merge {:time (first datapoint)} (second datapoint))))

(def csv-header (->> [:time :x :y :z] (map name) (join ",")))
(defn csv-row [{t :time {x :x y :y z :z} :position}] (join "," [t x y z]))
(def csv-row-data (map csv-row csv-data))

(defn write-csv [nrows path]
  (with-open [file (io/writer path)]
    (.write file (->> csv-row-data (take nrows) (cons csv-header) (join "\n")))))

(write-csv 100 "./output/test.csv")


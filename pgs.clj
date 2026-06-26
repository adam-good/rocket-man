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

(defn calculate-jerk 
  "Projectile Guidance System (PGS)\n
   Calculates the needed Jerk to guide the projectile to the target"
  [pos vel acc targ delta-t]
  (let [pos-diff (v3/elem-subtract pos targ)
        dt  (/ 1 delta-t)
        dt2 (/ 1 (* delta-t delta-t))
        dt3 (/ 1 (* delta-t delta-t delta-t))]
    (v3/elem-add
     (v3/scalar-product (* -2 dt3) pos-diff)
     (v3/scalar-product dt3 vel)
     (v3/scalar-product dt2 vel)
     (v3/scalar-product dt acc))))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
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
   #(phi/update-obj 
     % (calculate-jerk (:position %) (:velocity %) (:acceleration %) target dt)
     dt) 
   projectile ))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (utl/zip time-series (for [obj result] obj)))

(take 10 result)

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

(write-csv 50 "./output/test.csv")


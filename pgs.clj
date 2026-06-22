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
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 1e-3)))

;; Projectile Guidance System
(defn calculate-jerk 
  "Projectile Guidance System (PGS)\n
   Calculates the needed Jerk to guide the projectile to the target"
  [position velocity acceleration target delta-t] 
  (let [pos-diff (->> (v3/elem-subtract position target) 
                      (v3/normalize) 
                      (v3/scalar-product (* 0.1 (distance position target)) ))
        vel-next (phi/update-vector velocity acceleration delta-t)
        acc-next acceleration] ;; TODO: Find a better way to handle this err
    (->> (v3/elem-subtract
          (-> (/ 1 (* delta-t delta-t delta-t)) (v3/scalar-product pos-diff)) 
          (-> (/ 1 (* delta-t delta-t))         (v3/scalar-product vel-next))  
          (-> (/ 1 delta-t)                     (v3/scalar-product acc-next)))
         (v3/normalize)
         (v3/elem3-op #(min % 1))
         )))

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
   #(phi/update-obj % 
                    (calculate-jerk (:position %) (:velocity %) (:acceleration %) target dt) 
                    dt) 
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

(write-csv 50 "./output/test.csv")


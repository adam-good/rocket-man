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
(defn digitalize [x] 
  (cond
    (< x -0.99) -1
    (> x  0.99)  1
    :else       0))
(defn digitalize-vector [{x :x y :y z :z}]
  (v3/->Vector3
   (digitalize x)
   (digitalize y)
   (digitalize z)))

;; Projectile Guidance System
(defn calculate-jerk 
  "Projectile Guidance System (PGS)\n
   Calculates the needed Jerk to guide the projectile to the target"
  [{position :p velocity :v} target] 
  (let [direction  (->> (v3/elem-subtract target velocity) (v3/normalize))
        correction (v3/elem-subtract direction velocity)
        magnitude (->> (v3/elem-subtract position target)
                       (#(v3/elem3-op / %2 %1) velocity))]
    (->> correction (v3/normalize)
         #_(v3/elem-product magnitude)
         #_(digitalize-vector)
         (v3/scalar-product 2) )))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
(def projectile
  (phi/->PhysicalObj
   (v3/zero)             ; Position 
   (v3/->Vector3 1e-1 1e-1 1)             ; Velocity 
   (v3/zero)             ; Acceleration 
   1))
(def dt 0.05)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate #(phi/update-obj % (-> {:p (:position %) :v (:velocity %)} (calculate-jerk target)) dt) projectile))

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

(load-file "vector3.clj")
(load-file "utils.clj")

(ns physics
  (:require
   [vector3 :as v3]
   [utils]))

(defn get-differential [vector delta-time]
  (v3/scalar-product delta-time vector))

(defn apply-differential [vector differential]
  (v3/elem-add vector differential))

(defn update-vector [vector differential delta-time]
  (apply-differential vector (get-differential differential delta-time)))

(defrecord PhysicalObj
           [position velocity acceleration mass])

(defn update-obj
  ([object delta-time] (update-obj (v3/zero) object delta-time))
  ([object jerk delta-time]
   (->PhysicalObj
    (update-vector (:position object) (:velocity object) delta-time)
    (update-vector (:velocity object) (:acceleration object) delta-time)
    (update-vector (:acceleration object) jerk delta-time)
    (:mass object))))


 
(defn jerk [{position :p velocity :v} targ] 
  (let [direction (->> (v3/elem-subtract targ velocity) (v3/normalize))
        correction (v3/elem-subtract direction velocity)
        distance (->> (v3/elem-subtract targ position) (v3/magnitude))
        magnitude (/ 10 distance )]
    (->> correction (v3/normalize) (v3/scalar-product magnitude) )))

(def target (v3/->Vector3 1 1 1))
(def obj
  (->PhysicalObj
   (v3/zero)             ; Position 
   (v3/->Vector3 0 0 1)  ; Velocity 
   (v3/zero) ; Acceleration 
   1))
(def dt 0.05)
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate #(update-obj % (-> {:p (:position %) :v (:velocity %)} (jerk target)) dt) obj))
(def raw-data
  (utils/zip time-series (for [obj obj-series] obj)))

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

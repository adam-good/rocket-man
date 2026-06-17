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

(def target (v3/->Vector3 0 10 0))
(defn jerk [pos targ] 
  (->> (v3/elem-subtract targ pos) (v3/elem3-op #(min 1 %)) ))
(def obj 
  (->PhysicalObj 
   (v3/zero) ; Position 
   (v3/zero) ; Velocity 
   (v3/->Vector3 5 0 2.5) ; Acceleration 
   1))
(def dt 0.05)
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series 
  (iterate #(update-obj % (-> (:position %) (jerk target)) dt) obj))
(def raw-data
  (utils/zip time-series (for [obj obj-series] obj)))

;; (require '[clojure.data.csv :as csv] '[clojure.java.io :as io])
;; (def csv-data 
;;   (for [datapoint raw-data] (merge {:time (first datapoint)} (second datapoint))))
;; 
;; (defn write-csv [path data]
;;   (let [columns [:time :x :y :z]
;;         headers (map name columns)
;;         rows (mapv #(mapv % columns) data)]
;;     (with-open [file (io/write)])))
;; 
;; (take 10 csv-data)
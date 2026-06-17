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

(def obj
  (->PhysicalObj
   (v3/zero)            ;; Position
   (v3/zero)            ;; Velocity
   (v3/->Vector3 0 1 0) ;; Acceleration
   1))
(def dt 0.05)
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series (iterate #(update-obj % (v3/->Vector3 0 -1 0) dt) obj))

(take 20 (utils/zip time-series obj-series))

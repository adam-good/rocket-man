(load-file "vector3.clj")

(ns physics
  (:require 
   [vector3 :as v3]))

(defn get-differential [vector delta-time] 
  (v3/scalar-product delta-time vector))

(defn apply-differential [vector differential]
  (v3/elem-add vector differential))

(defn update-vector [vector differential delta-time]
  (apply-differential vector (get-differential differential delta-time)))

(defrecord PhysicalObj 
           [position velocity acceleration mass])

(defn update-obj [object delta-time]
  (->PhysicalObj 
   (update-vector (:position object) (:velocity object) delta-time)
   (update-vector (:velocity object) (:acceleration object) delta-time)
   (:acceleration object)
   (:mass object)))

(def obj
  (->PhysicalObj
   (v3/zero)
   (v3/zero)
   (v3/->Vector3 0 1 0)
   1))
(def dt 0.01)
(def time-domain (iterate #(+ % dt) 0.0))
(defn accel [t] (if (< t 0.1) (v3/Vector3 0.5 1.0 0) (v3/zero)))

(take 10 (iterate #(update-obj % dt) obj))
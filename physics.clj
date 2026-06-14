(load-file "vector3.clj")

(ns physics
  (:require 
   [vector3 :as v3]))

(defn get-differential [vector delta-time] 
  (v3/vector3-scalar-prod delta-time vector))

(defn apply-differential [vector differential]
  (v3/vector3-add vector differential))

(defn update-vector [vector derivative delta-time]
  (apply-differential vector (get-differential derivative delta-time)))

(defrecord PhysicalObj 
           [position velocity acceleration mass])

(defn update-obj [object delta-time]
  (->PhysicalObj 
   (update-vector (:position object) (:velocity object) delta-time)
   (update-vector (:velocity object) (:acceleration object) delta-time)
   (:acceleration object)
   (:mass object)))
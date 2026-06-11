(ns physics)

(defrecord Vector3 [x y z])

(defn vector3-add [w v]
  (->Vector3
   (+ (:x w) (:x v))
   (+ (:y w) (:y v))
   (+ (:z w) (:z v))
   ))

(defn vector3-sub [w v]
  (->Vector3 
   (- (:x w) (:x v))
   (- (:y w) (:y v))
   (- (:z w) (:z v))
   ))
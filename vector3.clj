(ns vector3)

(defrecord Vector3 [x y z])

(defn vector3-add
  "Elementwise vector addition"
  [u v]
  (->Vector3
   (+ (:x u) (:x v))
   (+ (:y u) (:y v))
   (+ (:z u) (:z v))))

(defn vector3-sub
  "Elementwise vector subtraction"
  [u v]
  (->Vector3
   (- (:x u) (:x v))
   (- (:y u) (:y v))
   (- (:z u) (:z v))))

(defn vector3-scalar-prod
  "Elementwise scalar-vector product"
  [s u]
  (->Vector3
   (* s (:x u))
   (* s (:y u))
   (* s (:z u))))

(defn vector3-elem-prod
  "Elementwise vector product"
  [u v]
  (->Vector3
   (* (:x u) (:x v))
   (* (:y u) (:y v))
   (* (:z u) (:z v))))

(defn vector3-dotprod
  "Vector dot product"
  [u v]
  (reduce + [(* (:x u) (:x v))
             (* (:y u) (:y v))
             (* (:z u) (:z v))]))

;; Testing Vector3
(comment
  (def u (->Vector3 1 2 3))
  (def v (->Vector3 3 2 1))
  (def s 5)
  
  (vector3-add u v)
  (vector3-sub u v)
  (vector3-scalar-prod s u)
  (vector3-elem-prod u v)
  (vector3-dotprod u v))
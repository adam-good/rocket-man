(ns vector3)

(defrecord Vector3 [x y z])
;
(defn elem-add
  "Elementwise vector addition"
  [u v]
  (->Vector3
   (+ (:x u) (:x v))
   (+ (:y u) (:y v))
   (+ (:z u) (:z v))))

(defn elem-subtract
  "Elementwise vector subtraction"
  [u v]
  (->Vector3
   (- (:x u) (:x v))
   (- (:y u) (:y v))
   (- (:z u) (:z v))))

(defn scalar-product
  "Elementwise scalar-vector product"
  [s u]
  (->Vector3
   (* s (:x u))
   (* s (:y u))
   (* s (:z u))))

(defn elem-product
  "Elementwise vector product"
  [u v]
  (->Vector3
   (* (:x u) (:x v))
   (* (:y u) (:y v))
   (* (:z u) (:z v))))

(defn dot-product
  "Vector Cartesian Product"
  [u v]
  (reduce + [(* (:x u) (:x v))
             (* (:y u) (:y v))
             (* (:z u) (:z v))]))

;; Testing Vector3
(comment
  (def u (->Vector3 1 2 3))
  (def v (->Vector3 3 2 1))
  (def s 5)
  
  (elem-add u v)
  (elem-subtract u v)
  (scalar-product s u)
  (elem-product u v)
  (dot-product u v))
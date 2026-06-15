(ns vector3)

(defrecord Vector3 [x y z])

(defn elem3-op 
  [op {ux :x uy :y uz :z} {vx :x vy :y vz :z} ] 
  (->Vector3
   (op ux vx)
   (op uy vy)
   (op uz vz)))

(defn elem-add
  "Elementwise vector addition"
  [u v]
  (elem3-op + u v))

(defn elem-subtract
  "Elementwise vector subtraction"
  [u v]
  (elem3-op - u v))

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
  (elem3-op * u v))

(defn elem-sum
  "Add x + y + z"
  [u]
  (let [{x :x y :y z :z} u]
    (+ x y z)))

(defn dot-product
  "Vector Cartesian Product"
  [u v]
  (elem-sum (elem-product u v)))

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
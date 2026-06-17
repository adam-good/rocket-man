(load-file "utils.clj")

(ns vector3
  (:require [utils]))

(defrecord Vector3 [x y z])

(defn zero [] (->Vector3 0 0 0))

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

(defn elem-product
  "Elementwise vector product"
  [u v]
  (elem3-op * u v))

(defn scalar-product
  "Elementwise scalar-vector product"
  [s u]
  (let [v (->Vector3 s s s)]
    (elem3-op * u v)))

(defn elem-sum
  "Add x + y + z"
  [u]
  (let [{x :x y :y z :z} u]
    (+ x y z)))

(defn dot-product
  "Vector Cartesian Product"
  [u v]
  (elem-sum (elem-product u v)))

(defn magnitude [{x :x y :y z :z}]
  (Math/sqrt (reduce + (map #(Math/pow % 2) [x y z]))))


(defn normal? [u] (utils/approx? (magnitude u) 1.0))

;; Testing Vector3
(comment
  (def u (->Vector3 1 2 3))
  (def v (->Vector3 3 2 1))
  (def s 5)

  (zero)

  (elem-add u v)
  (elem-subtract u v)
  (scalar-product s u)
  (elem-product u v)
  (dot-product u v)

  (def h (->> (Math/sqrt 2) (/ 1)))
  (def w (->Vector3 h h 0))
  (magnitude w)
  (normal? w)
  (normal? u)
  )
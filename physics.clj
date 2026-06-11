(ns physics)

(defrecord Vector3 [x y z])

(defn vector3-add
  "Elementwise vector addition"
  [w v]
  (->Vector3
   (+ (:x w) (:x v))
   (+ (:y w) (:y v))
   (+ (:z w) (:z v))))

(defn vector3-sub
  "Elementwise vector subtraction"
  [w v]
  (->Vector3
   (- (:x w) (:x v))
   (- (:y w) (:y v))
   (- (:z w) (:z v))))

(defn vector3-scalar-prod
  "Elementwise scalar-vector product"
  [s w]
  (->Vector3
   (* s (:x w))
   (* s (:y w))
   (* s (:z w))))

(defn enumerate [collection] (map-indexed vector collection))

(defn zip [w v] 
  (for [[i,a] (enumerate w) [j,b] (enumerate v) :when (= i j)] [a,b]))


;; Testing
(comment
  (def w (->Vector3 1 2 3))
  (def v (->Vector3 1 1 1))
  (def s 5)
  (vector3-add w v)
  (vector3-sub w v)
  (vector3-scalar-prod s w))

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

(defn vector3-elem-prod
  "Elementwise vector product"
  [w v]
  (->Vector3
   (* (:x w) (:x v))
   (* (:y w) (:y v))
   (* (:z w) (:z v))))

(defn vector3-dotprod
  "Vector dot product"
  [w v]
  (reduce + [(* (:x w) (:x v))
             (* (:y w) (:y v))
             (* (:z w) (:z v))]))

;; Testing Vector3
(comment
  (def w (->Vector3 1 2 3))
  (def v (->Vector3 3 2 1))
  (def s 5)
  
  (vector3-add w v)
  (vector3-sub w v)
  (vector3-scalar-prod s w)
  (vector3-elem-prod w v)
  (vector3-dotprod w v))

;; Generalized vector ops
(defn enumerate [collection] (map-indexed vector collection))

(defn zip [w v] 
  (for [[i,a] (enumerate w) [j,b] (enumerate v) :when (= i j)] [a,b]))

(defn elementwise-op [u v op]
  (map #(op (first %) (second %)) (zip u v)))

(defn vector-add 
  "Elementwise vector addition"
  [u v]
  (elementwise-op u v +))

(defn vector-sub
  "Elementwise vector subtraction"
  [u v]
  (elementwise-op u v -))

(defn vector-scalar-prod [s u]
  (map #(* % s) u))

(comment
  (def u [1 2 3])
  (def v [4 5 6])
  (def s 5)
  (vector-add u v)
  (vector-sub v u)
  (vector-scalar-prod s u))
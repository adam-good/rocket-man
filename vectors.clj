(ns vectors)

;; Generalized vector ops
(defn enumerate [collection] (map-indexed vector collection))

(defn zip [u v] 
  (for [[i,a] (enumerate u) [j,b] (enumerate v) :when (= i j)] [a,b]))

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

(defn dotprod
  "Vector dot product"
  [u v]
  (reduce + (elementwise-op u v *)))

(comment
  (def u [1 2 3])
  (def v [4 5 6])
  (def s 5)
  (vector-add u v)
  (vector-sub v u)
  (vector-scalar-prod s u)
  (dotprod u v))
(ns vectors)

;; Generalized vector ops
(defn zip [u v]
  (map vector u v))

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
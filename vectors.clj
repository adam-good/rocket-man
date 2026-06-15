(ns vectors)

;; Generalized vector ops
(defn zip [u v]
  (map vector u v))

(defn elementwise-op [u v op]
  (map #(op (first %) (second %)) (zip u v)))

(defn elem-add 
  "Elementwise vector addition"
  [u v]
  (elementwise-op u v +))

(defn elem-subtract
  "Elementwise vector subtraction"
  [u v]
  (elementwise-op u v -))

(defn scalar-product [s u]
  (map #(* % s) u))

(defn dot-product
  "Vector Cartesian Product"
  [u v]
  (reduce + (elementwise-op u v *)))

(comment
  (def u [1 2 3])
  (def v [4 5 6])
  (def s 5)
  (elem-add u v)
  (elem-subtract v u)
  (scalar-product s u)
  (dot-product u v))
(load-file "vector3.clj")
(load-file "utils.clj")

(ns order3
  (:require
   [vector3 :as v3]))

(defn c0 [r0] r0)
(defn c1 [v0] v0)
(defn c2 [a0] (/ a0 2))
(defn c3
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt}]
  (let [pos-coef (/ 1 N N N)
        vel-coef (/ -1 N N)
        acc-coef (/ -1 N)
        pos-diff (v3/elem-subtract rt r0)]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef v0)
     (v3/scalar-product acc-coef a0))))

(defn jerk-profile
  [N constraints]
  (v3/scalar-product 6 (c3 N constraints)))

(defn accel-profile
  [N constraints]
  (let [{a0 :a0} constraints]
    (v3/elem-add a0 (v3/scalar-product (* 6 N) (c3 N constraints)))))

(defn velocity-profile
  [N constraints]
  (let [{v0 :v0 a0 :a0} constraints]
    (v3/elem-add
     v0
     (v3/scalar-product N a0)
     (v3/scalar-product (* 3 N N) (c3 N constraints)))))

(defn jerk-crit-time [N] (v3/->Vector3 N N N)) ; Jerk is constant in this polynomial
(defn accel-crit-time
  [N constraints]
  (let [{a0 :a0} constraints
        neg-a0  (v3/scalar-product -1 a0)
        coef-c3 (v3/scalar-product 6 (c3 N constraints))]
    (v3/elem3-op / neg-a0 coef-c3)))

; TODO: This can go into another file probably
(defn jerk-peak [N constraints] (jerk-profile N constraints)) ; Jerk is constant in this polynomial
(defn accel-peak [N constraints]
  (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (jerk-crit-time N)]
    (v3/->Vector3
     (:x (accel-profile x-crit-time constraints))
     (:y (accel-profile y-crit-time constraints))
     (:z (accel-profile z-crit-time constraints)))))
(defn velocity-peak
  [N constraints]
  (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (accel-crit-time N constraints)]
    (v3/->Vector3
     (:x (velocity-profile x-crit-time constraints))
     (:y (velocity-profile y-crit-time constraints))
     (:z (velocity-profile z-crit-time constraints)))))

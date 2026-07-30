(load-file "vector3.clj")
(load-file "utils.clj")

(ns physics
  (:require
   [vector3 :as v3]
   [utils])
  (:require
   [utils :refer [unimplemented]]))

(defn get-differential [vector delta-time]
  (v3/scalar-product delta-time vector))

(defn apply-differential [vector differential]
  (v3/elem-add vector differential))

(defn update-vector [vector derivative delta-time]
  (apply-differential vector (get-differential derivative delta-time)))

(defrecord PhysicalObj
           [position velocity acceleration mass])

(defn update-obj
  ([object delta-time] (update-obj (v3/zero) object delta-time))
  ([object jerk delta-time]
   (->PhysicalObj
    (update-vector (:position object) (:velocity object) delta-time)
    (update-vector (:velocity object) (:acceleration object) delta-time)
    (update-vector (:acceleration object) jerk delta-time)
    (:mass object))))

;; Defining Polynomial Interpolation Constants
(defn c0-order3 [r0] r0)
(defn c0-order4 [] (unimplemented))
(defn c0-order5 [r0] r0)

(defn c1-order3 [v0] v0)
(defn c1-order4 [] (unimplemented))
(defn c1-order5 [v0] v0)

(defn c2-order3 [a0] (/ a0 2))
(defn c2-order4 [] (unimplemented)) ; TODO: Verify by hand
(defn c2-order5 [a0] (/ a0 2))

(defn c3-order3
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt}]
  (let [pos-coef (/  1 N N N)
        vel-coef (/ -1 N N)
        acc-coef (/ -1 N)
        pos-diff (v3/elem-subtract rt r0)]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef v0)
     (v3/scalar-product acc-coef a0))))
(defn c3-order4
  []
  (unimplemented))
(defn c3-order5
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt vt :vt at :at}]
  (let [pos-coef (/ 1 N N N)
        vel-coef (/ 1 N N)
        acc-coef (/ 1 2 N)
        pos-diff (v3/elem-add (v3/scalar-product 10 rt) (v3/scalar-product -10 r0))
        vel-diff (v3/elem-add (v3/scalar-product -9 vt) (v3/scalar-product -6 v0))
        acc-diff (v3/elem-add at (v3/scalar-product -3 a0))]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef vel-diff)
     (v3/scalar-product acc-coef acc-diff))))

(defn c4-order3 [] 0) ; Not used in rank 3
(defn c4-order4 [] (unimplemented))
(defn c4-order5
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt vt :vt at :at}]
  (let [pos-coef (/ 1 N N N N)
        vel-coef (/ 1 N N N)
        acc-coef (/ 1 2 N N)
        pos-diff (v3/elem-add (v3/scalar-product 15 r0) (v3/scalar-product -15 rt))
        vel-diff (v3/elem-add (v3/scalar-product 8 v0) (v3/scalar-product 7 vt))
        acc-diff (v3/elem-add (v3/scalar-product 3 a0) (v3/scalar-product -2 at))]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef vel-diff)
     (v3/scalar-product acc-coef acc-diff))))

(defn c5-order3 [] 0) ; Not used in rank 3
(defn c5-order4 [] 0) ; Not used in rank 4
(defn c5-order5
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt vt :vt at :at}]
  (let [pos-coef (/ 6 N N N N N)
        vel-coef (/ -3 N N N N)
        acc-coef (/ 1 N N)
        pos-diff (v3/elem-subtract rt r0)
        vel-diff (v3/elem-add v0 vt)
        acc-diff (v3/elem-subtract at a0)]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef vel-diff)
     (v3/scalar-product acc-coef acc-diff))))

(defn jerk-profile-order3
  [N constraints]
  (v3/scalar-product 6 (c3-order3 N constraints)))
(defn jerk-profile-order4 [] (unimplemented))
(defn jerk-profile-order5
  [N constraints]
  (v3/elem-add
   (v3/scalar-product 6 (c3-order5 N constraints))
   (v3/scalar-product (* 24 N) (c4-order5 N constraints))
   (v3/scalar-product (* 60 N N) (c5-order5 N constraints))))

(defn accel-profile-order3
  [N constraints]
  (let [{a0 :a0} constraints]
    (v3/elem-add a0 (v3/scalar-product (* 6 N) (c3-order3 N constraints)))))
(defn accel-profile-order4 [_N _constraints] (unimplemented))
(defn accel-profile-order5 [_N _constraints] (unimplemented))

(defn velocity-profile-order3
  [N constraints]
  (let [{v0 :v0 a0 :a0} constraints]
    (v3/elem-add
     v0
     (v3/scalar-product N a0)
     (v3/scalar-product (* 3 N N) (c3-order3 N constraints)))))
(defn velocity-profile-order4 [] (unimplemented))
(defn velocity-profile-order5 [] (unimplemented))

(defn jerk-crit-time-order3 [N] (v3/->Vector3 N N N))
(defn jerk-crit-time-order4 []  (unimplemented))
(defn jerk-crit-time-order5 []  (unimplemented))

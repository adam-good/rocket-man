(load-file "vector3.clj")
(load-file "physics.clj")
(load-file "utils.clj")
(load-file "json.clj")

(ns pgs
  (:require
   [clojure.math :as math]
   [vector3 :as v3]
   [physics :as phi]
   [utils   :as utl]
   [json    :as json]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 0.1)))


(defn c3 
  ([N r0 v0 a0 rt]
    (let [pos-coef (/ 1 (* N N N))
          vel-coef (/ 1 (* N N))
          acc-coef (/ 1 N)
          position (v3/elem-subtract rt r0)]
      (v3/elem-add
       (v3/scalar-product pos-coef position) 
       (v3/scalar-product (* -1 vel-coef) v0)
       (v3/scalar-product (* -1 acc-coef) a0))))
  ([N r0 v0 a0 rt vt at]
   (let [pos-coef (/ 1 (* N N N))
         vel-coef (/ 1 (* N N))
         acc-coef (/ 1 (* 2 N))
         position (v3/elem-add (v3/scalar-product 10 rt) (v3/scalar-product -10 r0))
         velocity (v3/elem-add (v3/scalar-product -9 vt) (v3/scalar-product  -6 v0))
         accel    (v3/elem-add  at                       (v3/scalar-product  -3 a0))]
     (v3/elem-add
      (v3/scalar-product pos-coef position)
      (v3/scalar-product vel-coef velocity) 
      (v3/scalar-product acc-coef accel)))))

(defn c4 [N r0 rt v0 vt a0 at]
  (let [pos-coef (/ 15 (* N N N N))
        vel-coef (/ 1 (* N N N))
        acc-coef (/ 1 (* 2 N N))
        position (v3/elem-subtract r0 rt)
        velocity (v3/elem-add (v3/scalar-product 8 v0) (v3/scalar-product  7 vt))
        accel    (v3/elem-add (v3/scalar-product 3 a0) (v3/scalar-product -2 at))]
    (v3/elem-add
     (v3/scalar-product pos-coef position)
     (v3/scalar-product vel-coef velocity)
     (v3/scalar-product acc-coef accel))))

(defn c5 [N r0 rt v0 vt a0 at]
  (let [pos-coef (/  6 (* N N N N N))
        vel-coef (/ -3 (* N N N N))
        acc-coef (/  1 (* N N))
        position (v3/elem-subtract rt r0)
        velocity (v3/elem-add v0 vt)
        accel    (v3/elem-subtract at a0)]
    (v3/elem-add
     (v3/scalar-product pos-coef position)
     (v3/scalar-product vel-coef velocity)
     (v3/scalar-product acc-coef accel))))

(defn jerk-profile
  ([N r0 v0 a0 rt]
   (v3/scalar-product 6 (c3 N r0 v0 a0 rt)))
  ([N r0 v0 a0 rt vt at]
   (v3/elem-add
    (v3/scalar-product 6 (c3 N r0 rt v0 vt a0 at))
    (v3/scalar-product (* 24 N) (c4 N r0 rt v0 vt a0 at))
    (v3/scalar-product (* 60 N N) (c5 N r0 rt v0 vt a0 at)))))

(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [N pos vel acc targ]
  (jerk-profile N pos vel acc targ))

(defn rand-neg1 [n] (-> (rand n) (- (/ n 2)) (* 2)))

;; Initial Conditions
(def target (v3/->Vector3 1 1 1))
(def projectile
  (phi/->PhysicalObj
   (v3/zero)               ; Position 
   (->> (v3/->Vector3 (rand-neg1 1) (rand-neg1 1) (rand-neg1 1) ) (v3/normalize))  ; Velocity 
   (v3/zero)               ; Acceleration 
   1))
(def dt 0.01)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate
   #(phi/update-obj %
                    (guidance-system
                     1
                     (:position %) (:velocity %) (:acceleration %) target) dt)
   projectile))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (for [[timestep, data] (utl/zip time-series result)]
    {:timestep timestep :datapoint data}))
(def dataset (take 300 raw-data))

(require '[clojure.java.io :as io])
(def json-data (json/write-str dataset))
(with-open [file (io/writer "./output/test.json")]
  (.write file json-data))




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
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 3e-2)))
(defn zero-does-not-exist [n] (if (== n 0) 1e-20 n))

(defrecord constraints [r0 v0 a0 rt vt at])

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

(defn accel-profile
  ([N r0 v0 a0 rt]
   (v3/elem-add a0 (v3/scalar-product (* 6 N) (c3 N r0 v0 a0 rt)))))

(defn velocity-profile
  ([N r0 v0 a0 rt]
   (v3/elem-add
    v0
    (v3/scalar-product N a0)
    (v3/scalar-product (* 3 N N) (c3 N r0 v0 a0 rt)))))

;; TODO: This needs fixed for non-constant jerk
(defn jerk-crit-time [N] (v3/->Vector3 N N N))

(defn accel-crit-time
  ([N r0 v0 a0 rt]
   (->> (v3/scalar-product 6 (c3 N r0 v0 a0 rt))
        (v3/elem3-op / (v3/scalar-product -1 a0)) (v3/elem3-op zero-does-not-exist))))

;; TODO: This needs fixed for non-constant jerk
(defn jerk-peak
  ([N r0 v0 a0 rt] (jerk-profile N r0 v0 a0 rt)))

(defn accel-peak
  ([N r0 v0 a0 rt]
   (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (jerk-crit-time N)]
     (v3/->Vector3 
      (:x (accel-profile x-crit-time r0 v0 a0 rt))
      (:y (accel-profile y-crit-time r0 v0 a0 rt))
      (:z (accel-profile z-crit-time r0 v0 a0 rt))))))

(defn vel-peak
  ([N r0 v0 a0 rt]
   (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (accel-crit-time N r0 v0 a0 rt)] 
     (v3/->Vector3 
      (:x (velocity-profile x-crit-time r0 v0 a0 rt))
      (:y (velocity-profile y-crit-time r0 v0 a0 rt))
      (:z (velocity-profile z-crit-time r0 v0 a0 rt))))))

(defn peak-values [N r0 v0 a0 rt]
  {:peak-jerk (jerk-peak  N r0 v0 a0 rt)
   :peak-acc  (accel-peak N r0 v0 a0 rt)
   :peak-vel  (vel-peak   N r0 v0 a0 rt)})

(defrecord max-values [max-jerk max-accel max-vel])
(defn n-mid [a b] (/ (+ a b) 2))
(defn t-is-valid? [{max-jerk  :max-jerk  max-accel :max-accel max-vel  :max-vel}
                   {peak-jerk :peak-jerk peak-accel  :peak-acc  peak-vel :peak-vel}]
  (and
   (v3/lt peak-jerk max-jerk)
   (v3/lt peak-accel max-accel)
   (v3/lt peak-vel max-vel)))
(defn search-target-time
  [{max-jerk :max-jerk max-accel :max-accel max-vel :max-vel} ;; max values
   t-min t-max tol
   {r0 :r0 v0 :v0 a0 :a0 rt :rt}] ; constraints
  (let [t-mid (n-mid t-min t-max)
        peak-vals   (peak-values t-mid r0 v0 a0 rt)
        max-vals    (->max-values max-jerk max-accel max-vel)
        constraints (->constraints r0 v0 a0 rt nil nil)]
    (cond
      (< (- t-max t-min) tol) t-mid
      (t-is-valid? max-vals peak-vals) (search-target-time max-vals t-min t-mid tol constraints)
      :else (search-target-time max-vals t-mid t-max tol constraints))))

(search-target-time {:max-jerk (v3/const 10) :max-accel (v3/const 10) :max-vel (v3/const 10)}
                    0.01 10 0.1
                    {:r0 (v3/->Vector3 0 0 0) :v0 (v3/->Vector3 0 0 1)
                     :a0 (v3/->Vector3 0 0 0) :rt (v3/->Vector3 1 1 1)})

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
   (v3/->Vector3 0 0 0.2)  ; Velocity 
   (v3/zero)               ; Acceleration 
   1))
(def dt 0.01)

;; Series Defnitions
(def time-series (iterate #(+ dt %) 0.0))
(def obj-series
  (iterate
   #(phi/update-obj %
                    (guidance-system
                     (search-target-time
                      {:max-jerk (v3/const 8) :max-accel (v3/const 16) :max-vel (v3/const 32)}
                      0.01 10 0.1
                      {:r0 (:position %) :v0 (:velocity %) :a0 (:acceleration %) :rt target})
                     (:position %) (:velocity %) (:acceleration %) target) dt)
   projectile))

;; Limit Results
(def result (take-while #(impact? % target) obj-series))
(def raw-data
  (for [[timestep, data] (utl/zip time-series result)]
    {:timestep timestep :datapoint data}))
(def dataset (take 600 raw-data))

(require '[clojure.java.io :as io])
(def json-data (json/write-str dataset))
(with-open [file (io/writer "./output/test.json")]
  (.write file json-data))




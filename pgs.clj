(load-file "vector3.clj")
(load-file "physics.clj")
(load-file "utils.clj")
(load-file "json.clj")

(ns pgs
  (:require
   [vector3 :as v3]
   [physics :as phi]
   [utils   :as utl]
   [json    :as json])
  (:require
   [utils :only [unimplemented]]))

;; Helper Functions
(defn distance [u v] (->> (v3/elem-subtract v u) (v3/magnitude) (abs)))
(defn impact? [projectile target] (->> (:position projectile) (distance target) (< 3e-2)))
(defn zero-does-not-exist [n] (if (== n 0) 1e-20 n))

(defn target-rank1? [constraints]
  (and
   (contains? constraints :rt)
   (not (contains? constraints :vt))
   (not (contains? constraints :at))))
(defn target-rank2? [constraints]
  (and
   (contains? constraints :rt)
   (contains? constraints :vt)
   (not (contains? constraints :at))))

(defn target-rank3? [constraints]
  (and
   (contains? constraints :rt)
   (contains? constraints :vt)
   (contains? constraints :at)))

(defn constraint-dispatch [_N constraints]
  (cond
    (target-rank1? constraints) ::rank1
    (target-rank2? constraints) ::rank2
    (target-rank3? constraints) ::rank3))

;; TODO: This might be simplified after I solve other cases
(defmulti  c0 constraint-dispatch)
(defmethod c0 ::rank1 [_N {r0 :r0}] r0)
(defmethod c0 ::rank2 [_N {_r0 :r0}] (unimplemented))
(defmethod c0 ::rank3 [_N {r0 :r0}] r0)

(defmulti  c1 constraint-dispatch)
(defmethod c1 ::rank1 [_N {v0 :v0}] v0)
(defmethod c1 ::rank2 [_N {_v0 :v0}] (unimplemented))
(defmethod c1 ::rank3 [_N {v0 :v0}] v0)

(defmulti  c2 constraint-dispatch)
(defmethod c2 ::rank1 [_N {a0 :a0}] (/ a0 2))
(defmethod c2 ::rank2 [_N {_a0 :a0}] (unimplemented))
(defmethod c2 ::rank3 [_N {a0 :a0}] (/ a0 2))

(defmulti c3 constraint-dispatch)
(defmethod c3 ::rank1
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt}]
  (let [pos-coef (/  1 N N N)
        vel-coef (/ -1 N N)
        acc-coef (/ -1 N)
        pos-diff (v3/elem-subtract rt r0)]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef v0)
     (v3/scalar-product acc-coef a0))))
(defmethod c3 ::rank2
  [_N {_r0 :r0 _v0 :v0 _a0 :a0 _rt :rt _vt :vt}]
  :unimplemented)
(defmethod c3 ::rank3
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt vt :vt at :at}]
  (let [pos-coef (/ 1 N N N)
        vel-coef (/ 1 N N)
        acc-coef (/ 1 2 N)
        pos-diff (v3/elem-add (v3/scalar-product 10 rt) (v3/scalar-product -10 r0))
        vel-diff (v3/elem-add (v3/scalar-product -9 vt) (v3/scalar-product -6 v0))
        acc-diff (v3/elem-add at                        (v3/scalar-product -3 a0))]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef vel-diff)
     (v3/scalar-product acc-coef acc-diff))))

(defmulti  c4 constraint-dispatch)
(defmethod c4 ::rank1 [_N {}] 0) ; Rank 1 uses a rank3 polynomial
(defmethod c4 ::rank2 [_N {}] (unimplemented))
(defmethod c4 ::rank3
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

(defmulti  c5 constraint-dispatch)
(defmethod c5 ::rank1 [_N _map] 0)
(defmethod c5 ::rank2 [_N _map] (unimplemented))
(defmethod c5 ::rank3
  [N {r0 :r0 v0 :v0 a0 :a0 rt :rt vt :vt at :at}]
  (let [pos-coef (/  6 N N N N N)
        vel-coef (/ -3 N N N N)
        acc-coef (/  1 N N)
        pos-diff (v3/elem-subtract rt r0)
        vel-diff (v3/elem-add v0 vt)
        acc-diff (v3/elem-subtract at a0)]
    (v3/elem-add
     (v3/scalar-product pos-coef pos-diff)
     (v3/scalar-product vel-coef vel-diff)
     (v3/scalar-product acc-coef acc-diff))))

(defmulti  jerk-profile constraint-dispatch)
(defmethod jerk-profile ::rank1
  [N constraints] (v3/scalar-product 6 (c3 N constraints)))
(defmethod jerk-profile ::rank2 [_N _constraints] (unimplemented))
(defmethod jerk-profile ::rank3
  [N constraints]
  (v3/elem-add
   (v3/scalar-product 6 (c3 N constraints))
   (v3/scalar-product (* 24 N) (c4 N constraints))
   (v3/scalar-product (* 60 N N) (c4 N constraints))))

(defmulti accel-profile constraint-dispatch)
(defmethod accel-profile ::rank1 [N constraints]
  (let [{a0 :a0} constraints]
    (v3/elem-add a0 (v3/scalar-product (* 6 N) (c3 N constraints)))))
(defmethod accel-profile ::rank2 [_N _constraints] (unimplemented))
(defmethod accel-profile ::rank2 [_N _constraints] (unimplemented))

(defmulti velocity-profile constraint-dispatch)
(defmethod velocity-profile ::rank1
  [N constraints]
  (let [{v0 :v0 a0 :a0} constraints]
    (v3/elem-add
     v0
     (v3/scalar-product N a0)
     (v3/scalar-product (* 3 N N) (c3 N constraints)))))
(defmethod velocity-profile ::rank2 [_N _constraints] (unimplemented))
(defmethod velocity-profile ::rank3 [_N _constraints] (unimplemented))

;; TODO: This needs fixed for non-constant jerk
(defn jerk-crit-time [N] (v3/->Vector3 N N N))

(defmulti accel-crit-time constraint-dispatch)
(defmethod accel-crit-time ::rank1
  [N constraints]
  (let [{a0 :a0} constraints]
    (->> (v3/scalar-product 6 (c3 N constraints))
         (v3/elem3-op / (v3/scalar-product -1 a0))
         (v3/elem3-op zero-does-not-exist))))
(defmethod accel-crit-time ::rank2 [_N _constraints] (unimplemented))
(defmethod accel-crit-time ::rank3 [_N _constraints] (unimplemented))

;; TODO: This needs fixed for non-constant jerk
(defn jerk-peak
  ([N constraints] (jerk-profile N constraints)))

(defn accel-peak
  ([N constraints]
   (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (jerk-crit-time N)]
     (v3/->Vector3
      (:x (accel-profile x-crit-time constraints))
      (:y (accel-profile y-crit-time constraints))
      (:z (accel-profile z-crit-time constraints))))))

(defn vel-peak
  ([N constraints]
   (let [{x-crit-time :x y-crit-time :y z-crit-time :z} (accel-crit-time N constraints)]
     (v3/->Vector3
      (:x (velocity-profile x-crit-time constraints))
      (:y (velocity-profile y-crit-time constraints))
      (:z (velocity-profile z-crit-time constraints))))))

(defn peak-values [N constraints]
  {:peak-jerk (jerk-peak  N constraints)
   :peak-acc  (accel-peak N constraints)
   :peak-vel  (vel-peak   N constraints)})

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
   constraints] ; constraints
  (let [t-mid (n-mid t-min t-max)
        peak-vals   (peak-values t-mid constraints)
        max-vals    (->max-values max-jerk max-accel max-vel)]
    (cond
      (< (- t-max t-min) tol) t-mid
      (t-is-valid? max-vals peak-vals) (search-target-time max-vals t-min t-mid tol constraints)
      :else (search-target-time max-vals t-mid t-max tol constraints))))

(defn guidance-system
  "Projectile Guidance System (PGS)\n
     Calculates the needed Jerk to guide the projectile to the target"
  [N pos vel acc targ]
  (jerk-profile N {:r0 pos :v0 vel :a0 acc :rt targ}))

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




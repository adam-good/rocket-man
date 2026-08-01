(load-file "physics/poly-int/order3.clj")
(load-file "physics/poly-int/order4.clj")
(load-file "physics/poly-int/order5.clj")
(load-file "utils")

(ns physics.poly-int
  (:require
    [physics.poly-int.order3 :as odr3]
    [physics.poly-int.order4 :as odr4]
    [physics.poly-int.order5 :as odr5]
    [utils :refer [contains-all?]]))

; TODO: Find a more abstract way to do this
(defn get-order [_N constraints]
  (cond 
    (contains-all? constraints :r0 :v0 :a0 :rt)         ::order3
    (contains-all? constraints :r0 :v0 :a0 :rt :vt)     ::order4
    (contains-all? constraints :r0 :v0 :a0 :rt :vt :at) ::order5))

(defmulti  c0 get-order)
(defmethod c0 ::order3 [N constraints] (odr3/c0 N constraints))
(defmethod c0 ::order4 [N constraints] (odr4/c0 N constraints))
(defmethod c0 ::order5 [N constraints] (odr5/c0 N constraints))

(defmulti  c1 get-order)
(defmethod c1 ::order3 [N constraints] (odr3/c1 N constraints))
(defmethod c1 ::order4 [N constraints] (odr4/c1 N constraints))
(defmethod c1 ::order5 [N constraints] (odr5/c1 N constraints))

(defmulti  c2 get-order)
(defmethod c2 ::order3 [N constraints] (odr3/c2 N constraints))
(defmethod c2 ::order4 [N constraints] (odr4/c2 N constraints))
(defmethod c2 ::order5 [N constraints] (odr5/c2 N constraints))

(defmulti  c3 get-order)
(defmethod c3 ::order3 [N constraints] (odr3/c3 N constraints))
(defmethod c3 ::order4 [N constraints] (odr4/c3 N constraints))
(defmethod c3 ::order5 [N constraints] (odr5/c3 N constraints))

(defmulti  c4 get-order)
(defmethod c4 ::order3 [N constraints] (odr3/c4 N constraints))
(defmethod c4 ::order4 [N constraints] (odr4/c4 N constraints))
(defmethod c4 ::order5 [N constraints] (odr5/c4 N constraints))

(defmulti  c5 get-order)
(defmethod c5 ::order3 [N constraints] (odr3/c5 N constraints)) 
(defmethod c5 ::order4 [N constraints] (odr4/c5 N constraints))
(defmethod c5 ::order5 [N constraints] (odr5/c5 N constraints))

(defmulti  jerk-profile get-order)
(defmethod jerk-profile ::order3 [N constr] (odr3/jerk-profile N constr))
(defmethod jerk-profile ::order4 [N constr] (odr4/jerk-profile N constr))
(defmethod jerk-profile ::order5 [N constr] (odr5/jerk-profile N constr))

(defmulti  accel-profile get-order)
(defmethod accel-profile ::order3 [N constr] (odr3/accel-profile N constr))
(defmethod accel-profile ::order4 [N constr] (odr4/accel-profile N constr))
(defmethod accel-profile ::order5 [N constr] (odr5/accel-profile N constr))

(defmulti  velocity-profile get-order)
(defmethod velocity-profile ::order3 [N constr] (odr3/velocity-profile N constr))
(defmethod velocity-profile ::order4 [N constr] (odr4/velocity-profile N constr))
(defmethod velocity-profile ::order5 [N constr] (odr5/velocity-profile N constr))

(defmulti  jerk-crit-time get-order)
(defmethod jerk-crit-time ::order3 [N constr] (odr3/jerk-crit-time N constr))
(defmethod jerk-crit-time ::order4 [N constr] (odr4/jerk-crit-time N constr))
(defmethod jerk-crit-time ::order5 [N constr] (odr5/jerk-crit-time N constr))

(defmulti  accel-crit-time get-order)
(defmethod accel-crit-time ::order3 [N constr] (odr3/accel-crit-time N constr))
(defmethod accel-crit-time ::order4 [N constr] (odr4/accel-crit-time N constr))
(defmethod accel-crit-time ::order5 [N constr] (odr5/accel-crit-time N constr))

(defmulti  jerk-peak get-order)
(defmethod jerk-peak ::order3 [N constr] (odr3/jerk-peak N constr))
(defmethod jerk-peak ::order4 [N constr] (odr4/jerk-peak N constr))
(defmethod jerk-peak ::order5 [N constr] (odr5/jerk-peak N constr))

(defmulti  accel-peak get-order)
(defmethod accel-peak ::order3 [N constr] (odr3/accel-peak N constr))
(defmethod accel-peak ::order4 [N constr] (odr4/accel-peak N constr))
(defmethod accel-peak ::order5 [N constr] (odr5/accel-peak N constr))

(defmulti  velocity-peak get-order)
(defmethod velocity-peak ::order3 [N constr] (odr3/velocity-peak N constr))
(defmethod velocity-peak ::order4 [N constr] (odr4/velocity-peak N constr))
(defmethod velocity-peak ::order5 [N constr] (odr5/velocity-peak N constr))



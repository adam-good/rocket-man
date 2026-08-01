(load-file "vector3.clj")
(load-file "utils.clj")

(ns physics.poly-int.order4
  (:require 
    [vector3 :as v3]
    [utils :refer [unimplemented]]))

(defn c0 [_ _] (unimplemented))
(defn c1 [_ _] (unimplemented))
(defn c2 [_ _] (unimplemented))
(defn c3 [_ _] (unimplemented))
(defn c4 [_ _] (unimplemented))
(defn c5 [_ _] 0) ; c5 not in Order-4 Polynomial
(defn jerk-profile [_ _] (unimplemented))
(defn accel-profile [_ _] (unimplemented))
(defn velocity-profile [_ _] (unimplemented))
(defn jerk-crit-time [_ _] (unimplemented))
(defn accel-crit-time [_ _] (unimplemented))
(defn jerk-peak [_ _] (unimplemented))
(defn accel-peak [_ _] (unimplemented))
(defn velocity-peak [_ _] (unimplemented))

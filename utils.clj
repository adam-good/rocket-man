(ns utils)

(defn zip
  "Zips to collections into a vector of element pairs"
  [u v]
  (map vector u v))

(defn approx?
  "Returns non-nil if a,b are within epsilon of each other"
  ([a b] (approx? a b 5.96e-08))
  ([a b eps]
   (cond
     (and (== a 0) (== b 0)) true
     (and (== a 0) (not= b 0)) false
     (and (not= a 0) (== b 0)) false 
     :else (-> (/ a b) (Math/abs) (- 1.0) (< eps)))))
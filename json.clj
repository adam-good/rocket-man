(ns json
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [join]]))

(derive java.util.Map ::collection)

(defn key-to-string [key]
  (str (name key) ":"))

(defmulti to-json class)
(defmethod to-json :default      [data] (str data))
(defmethod to-json ::collection  [data]
  (->>
   (for [[key value] data] (str "{" (key-to-string key) (to-json value) "}"))
   (join ",")))


(def data {:a "A" :b {:c "C"}})
(to-json data)



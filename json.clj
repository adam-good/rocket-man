(ns json
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [join]]))

(derive java.util.Map       ::object)
(derive java.util.ArrayList ::array)
(derive java.lang.Long      ::number)
(derive java.lang.Double    ::number)
(derive java.lang.String    ::string)
(derive java.lang.Character ::string)
(derive java.lang.Boolean   ::bool)

(defn bracketize-curly [s] (str "{" s "}"))
(defn bracketize-square [s] (str "[" s "]"))
(defn quoteize [s] (str "\"" s "\"") )

(defn key-to-string [key]
  (str (name key)))

(defn create-json-pair [key value]
  (str 
   (-> (key-to-string key) (quoteize))
   " : " value))

(defmulti to-json-string class)
(defmethod to-json-string :default [data] (str data))
(defmethod to-json-string ::bool   [data] (str data)) ; TODO: Let default do this?
(defmethod to-json-string ::string [data] (quoteize data))
(defmethod to-json-string ::number [data] (str data))
(defmethod to-json-string ::array       [data] 
  (->>
   (map to-json-string data)
   (join ",")
   (bracketize-square)))
(defmethod to-json-string ::object  [data]
  (->>
   (for [[key value] data]
     (create-json-pair key (to-json-string value)))
   (join ",")
   (bracketize-curly)))


(def data
  [
   {:a "A" :b "B" :c {:d 123}}
   {:e "E" :f {:g "G"} :h true}
] )
(def json-str (to-json-string data))
(println json-str)

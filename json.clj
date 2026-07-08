(ns json
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [join]]))

(derive java.util.Map ::collection)
(derive java.util.ArrayList ::array)

(defn bracketize-curly [s] (str "{" s "}"))
(defn bracketize-square [s] (str "[" s "]"))
(defn quoteize [s] (str "\"" s "\"") )

(defn key-to-string [key]
  (str (name key)))

(defn create-json-pair [key value]
  (str 
   (-> (key-to-string key) (quoteize))  
   ":"  
   (-> value quoteize) ))

(defmulti to-json-string class)
(defmethod to-json-string :default      [data] (str data))
(defmethod to-json-string ::array       [data] 
  (->>
   (map to-json-string data)
   (join ",")
   (bracketize-square)))
(defmethod to-json-string ::collection  [data]
  (->>
   (for [[key value] data]
     (create-json-pair key (to-json-string value)))
   (join ",")
   (bracketize-curly)))


(def data
  [
   {:a "A" :b "B" :c {:d "D"}}
   {:e "E" :f {:g "G"} :h "H"}
] )
(def json-str (to-json-string data))
(println json-str)

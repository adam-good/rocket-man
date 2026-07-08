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

(defmulti to-json class)
(defmethod to-json :default      [data] (str data))
(defmethod to-json ::array       [data] 
  (->>
   (map to-json data)
   (join ",")
   (bracketize-square)))
(defmethod to-json ::collection  [data]
  (->>
   (for [[key value] data]
     (create-json-pair key (to-json value)))
   (join ",")
   (bracketize-curly)))


(def data
  [
   {:a "A" :b "B" :c {:d "D"}}
   {:e "E" :f {:g "G"} :h "H"}
] )
(def json-str (to-json data))
(println json-str)
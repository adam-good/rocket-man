(ns json
  (:require
   [clojure.string :refer [join]]))

(defn json-type [item]
  (cond
    (boolean? item)    :bool
    (string?  item)    :string
    (number?  item)    :number
    (sequential? item) :array
    (map? item)        :object))

(defn bracketize-curly [s] (str "{" s "}"))
(defn bracketize-square [s] (str "[" s "]"))
(defn quoteize [s] (str "\"" s "\""))

(defn key-to-string [key]
  (str (name key)))

(defn create-json-pair [key value]
  (str
   (-> (key-to-string key) (quoteize))
   " : " value))

(defmulti to-json-string json-type)
(defmethod to-json-string :default [data] (str data))
(defmethod to-json-string :bool    [data] (str data)) ; TODO: Let default do this?
(defmethod to-json-string :string  [data] (quoteize data))
(defmethod to-json-string :number  [data] (str data))
(defmethod to-json-string :array   [data]
  (->>
   (map to-json-string data)
   (join ",")
   (bracketize-square)))
(defmethod to-json-string :object  [data]
  (->>
   (for [[key value] data]
     (create-json-pair key (to-json-string value)))
   (join ",")
   (bracketize-curly)))


(def data 
  {:data [{:a "A" :b "B" :c {:d 123}} {:e "E" :f {:g "G"} :h true}]})
(def json-str (to-json-string data))
(println json-str)

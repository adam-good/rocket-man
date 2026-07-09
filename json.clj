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
   ":" value))

(defmulti serialize json-type)
(defmethod serialize :default [data] (str data))
(defmethod serialize :bool    [data] (str data)) ; TODO: Let default do this?
(defmethod serialize :string  [data] (quoteize data))
(defmethod serialize :number  [data] (str data))
(defmethod serialize :array   [data]
  (->>
   (map serialize data)
   (join ",")
   (bracketize-square)))
(defmethod serialize :object  [data]
  (->>
   (for [[key value] data]
     (create-json-pair key (serialize value)))
   (join ",")
   (bracketize-curly)))

(defmulti  write-str json-type)
(defmethod write-str :object [data] (serialize data))
(defmethod write-str :default [data] (serialize {:data data}))

(comment 
  (def data [{:a "A" :b "B" :c {:d 123}} {:e "E" :f {:g "G"} :h true}])
  (def json-str (write-str data)) 
  (println json-str))

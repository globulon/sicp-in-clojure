(ns sicp.generic-arithmetic)

(defn tagged[data type]
  (with-meta data {:type-of type}))

(defn tag-in[data]
  (:type-of (meta data)))

(defn selected-types [x y]
  [(tag-in x) (tag-in y)])

(defmulti add selected-types )
(defmulti sub selected-types )
(defmulti mul selected-types )
(defmulti div selected-types )
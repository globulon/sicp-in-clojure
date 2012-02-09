(ns sicp.clojure-number
  (:use sicp.generic-arithmetic))

(def clj-type :clojure)

(defn- clj-data [x]
  (if (= clj-type (tag-in x))
    (:value x)
    x))

(defn make-clj [data]
  (if (number? data)
    (tagged {:value data} clj-type)
    data))

(defmethod add [:clojure :clojure] [x y]
  (make-clj (+ (clj-data x) (clj-data y))))

(defmethod sub [:clojure :clojure] [x y]
  (make-clj (- (clj-data x) (clj-data y))))

(defmethod mul [:clojure :clojure] [x y]
  (make-clj (* (clj-data x) (clj-data y))))

(defmethod div [:clojure :clojure] [x y]
  (make-clj (/ (clj-data x) (clj-data y))))




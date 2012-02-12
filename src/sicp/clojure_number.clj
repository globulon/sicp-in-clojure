(ns sicp.clojure-number
  ^ {:doc "Supposedly match ex 2.78"}
  (:use sicp.generic-arithmetic))

(def clj-type ::clojure)

(defn clj-data [x]
  (if (= clj-type (tag-in x))
    (:value x)
    x))

(defn- equal? [x y]
  (= (:value x) (:value y)))

(defn make-clj [data]
  (if (integer? data)
    (tagged {:value data} clj-type)
    data))

(defmethod add [clj-type clj-type] [x y]
  (make-clj (+ (clj-data x) (clj-data y))))

(defmethod sub [clj-type clj-type] [x y]
  (make-clj (- (clj-data x) (clj-data y))))

(defmethod mul [clj-type clj-type] [x y]
  (make-clj (* (clj-data x) (clj-data y))))

(defmethod div [clj-type clj-type] [x y]
  (make-clj (/ (clj-data x) (clj-data y))))

(defmethod equ? [clj-type clj-type] [x y]
  (equal? x y))
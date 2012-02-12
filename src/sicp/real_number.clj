(ns sicp.real-number
  (:use sicp.generic-arithmetic))

(def real-type ::real)

(defn make-real [x]
  (if (number? x)
    (tagged {:value x} real-type)))

(defn real-value [x]
  (if (= real-type (tag-in x))
    (:value x)
    x))

(defn- apply-op[exp x y]
  (make-real (apply exp (map :value [x y]))))

(defmethod add [real-type real-type] [x y]
  (apply-op + x y))

(defmethod mul [real-type real-type] [x y]
  (apply-op * x y))

(defmethod sub   [real-type real-type] [x y]
  (apply-op - x y))

(defmethod div   [real-type real-type] [x y]
  (apply-op / x y))

(defmethod equ?  [real-type real-type] [x y]
  (= x y))

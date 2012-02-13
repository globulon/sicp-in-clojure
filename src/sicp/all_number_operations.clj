(ns sicp.all-number-operations
  ^{:doc "Install all numbers library in order to have all
          generic operations covers problems on "}
  (:use [sicp
         core
         generic-arithmetic
         clojure-number
         complex
         complex-polar
         complex-rectangular
         rational
         real-number]))

(remove-all-methods higher-type-in)

(def rectangular-datum (rectangular-complex))
(def polar-datum (polar-complex))

(defmethod make-from-real-imag :real-imag-constructor [x y]
  (make-complex (make-from-real-imag-constructor rectangular-datum x y)))

(defmethod make-from-mag-ang :mag-ang-constructor [r theta]
  (make-complex (make-from-mag-ang-constructor polar-datum r theta)))

(def tower {clj-type 0 rat-type 1 real-type 2 complex-type 3})

(defmethod higher-type-in :default [& xs]
  (key (first
    (max-key val
      (select-keys tower (map tag-in xs))))))

(defmethod raise clj-type [x]
  (make-rat (clj-data x) 1))

(defmethod raise rat-type [x]
  (make-real (double (/ (numer x) (denum x)))))

(defmethod raise real-type [x]
  (make-from-real-imag (real-value x) 0))

(defmethod raise complex-type [x] x)







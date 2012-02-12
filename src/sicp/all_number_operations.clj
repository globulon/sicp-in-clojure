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
         rational]))

(def tower [clj-number rat-type complexe-type])



(defmulti raise (fn [x] (tag-in x)))

(defmethod raise clj-type [x]
  (make-rat (clj-data x) 1))

(defmethod raise clj-type [x]
  (make-rat (clj-data x) 1))



;(defmethod add [complex-type clj-type] [x y]
;  (add ))

;(defmulti add   selected-types)
;(defmulti sub   selected-types)
;(defmulti mul   selected-types)
;(defmulti div   selected-types)
;(defmulti equ?  selected-types)






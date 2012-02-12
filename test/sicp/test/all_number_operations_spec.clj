(ns sicp.test.all-number-operations-spec
  ^{:doc "supposedly complete exercise 2.83 and 2.84"}
  (:use clojure.test)
  (:use [sicp
         core
         generic-arithmetic
         clojure-number
         complex
         complex-polar
         complex-rectangular
         rational
         real-number
         all-number-operations]))

;(deftest add-integer-with-complex-should-create-a-new-complex
;  (is (= (make-from-real-imag 3 4) (add (make-clj 1) (make-from-real-imag 2 4)))))

(deftest raise-should-promote-integer-to-rational
  (is (= (make-rat 2 1) (raise (make-clj 2)))))

(deftest raise-should-promote-rational-to-real
  (is (= (make-real 2.0) (raise (make-rat 2 1)))))

(deftest raise-should-promote-real-to-complex
  (is (= (make-from-real-imag 2 0)  (raise (make-real 2)))))

(deftest raise-to-complex-integer-should-provide-complex-from-integer
  (is (= (make-from-real-imag 2.0 0) (raise-to complex-type (make-clj 2)))))

(deftest raise-to-rat-integer-should-provide-rat-from-integer
  (is (= (make-rat 2 1) (raise-to rat-type (make-clj 2)))))

(deftest raise-to-real-integer-should-provide-real-from-integer
  (is (= (make-real 2.0) (raise-to real-type (make-clj 2)))))



(run-tests)
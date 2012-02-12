(ns sicp.test.real-number-spec
  (:use clojure.test)
  (:use sicp.generic-arithmetic)
  (:use sicp.real-number))

(deftest add-should-add-two-real-numbers
  (is (= (make-real 3) (add (make-real 1) (make-real 2)))))

(deftest mul-should-mul-two-real-numbers
  (is (= (make-real 21) (mul (make-real 3) (make-real 7)))))

(deftest sub-should-mul-two-real-numbers
  (is (= (make-real 8) (sub (make-real 19) (make-real 11)))))

(deftest div-should-mul-two-real-numbers
  (is (= (make-real 3) (div (make-real 21) (make-real 7)))))

(deftest equ?-should-return-true-on-equality
  (is (equ? (make-real 7) (make-real 7))))

(deftest equ?-should-return-false-on-no-equality
  (is (not (equ? (make-real 7) (make-real 5)))))

;(run-tests )
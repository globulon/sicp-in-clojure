(ns sicp.test.rational-spec
  (:use sicp.generic-arithmetic)
  (:use sicp.rational)
  (:use clojure.test))

(deftest tag-in-rational-should-be-identifiable
  (is (= rat-type (tag-in (make-rat 1 2)))))

(deftest add-two-rational-should-produce-result
  (is (= (make-rat 3 4) (add (make-rat 1 2) (make-rat 1 4))))
  (is (= rat-type (tag-in (add (make-rat 1 2) (make-rat 1 4))))))

(deftest mul-two-rational-should-produce-result
  (is (= (make-rat  1 6) (mul (make-rat 1 3) (make-rat 1 2))))
  (is (= rat-type (tag-in (mul (make-rat 1 3) (make-rat 1 2))))))

(deftest sub-two-rational-should-produce-result
  (is (= (make-rat  1 4) (sub (make-rat 3 4) (make-rat 1 2))))
  (is (= rat-type (tag-in (sub (make-rat 3 4) (make-rat 1 2))))))

(deftest div-two-rational-should-produce-result
  (is (= (make-rat  1 2) (div (make-rat 1 1) (make-rat 2 1))))
  (is (= rat-type (tag-in (div (make-rat 1 1) (make-rat 2 1))))))

(deftest equ-should-return-true-when-rationals-are-equals
  (is (equ? (make-rat 1 2) (make-rat 1 2))))

(deftest equ-should-return-false-when-rationals-are-not-equals
  (is (not (equ? (make-rat 1 2) (make-rat 3 4)))))


(run-tests)
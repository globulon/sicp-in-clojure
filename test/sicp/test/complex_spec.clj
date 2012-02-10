(ns sicp.test.complex-spec
  (:use [sicp generic-arithmetic core complex complex-rectangular complex-polar])
  (:use clojure.test))

(def rectangular-datum (rectangular-complex))
(def polar-datum (polar-complex))

(defmethod make-from-real-imag :real-imag-constructor [x y]
  (make-from-real-imag-constructor rectangular-datum x y))

(defmethod make-from-mag-ang :mag-ang-constructor [r theta]
  (make-from-mag-ang-constructor polar-datum r theta))

(deftest make-from-real-imag-with-dispatch-should-create-rectangular-coordinate
  (is (= (->Rectangular 1 1) (make-from-real-imag 1 1))))

(deftest make-from-mag-ang-with-dispatch-should-create-rectangular-coordinate
  (is (= (->Polar 1 1) (make-from-mag-ang 1 1))))

(deftest add-complex-should-produce-rectangular-as-sum-of-real-imag
  (is (=  (->Rectangular 3 3)
        (add
          (make-from-real-imag 1 1)
          (make-from-real-imag 2 2)))))

(deftest sub-complex-should-produce-rectangular-as-sum-of-real-imag
  (is (=  (->Rectangular 2 2)
        (sub
          (make-from-real-imag 3 3)
          (make-from-real-imag 1 1)))))

(deftest mul-complex-should-produce-rectangular-as-sum-of-real-imag
  (is (=  (->Polar 6 0.9)
        (mul
          (make-from-mag-ang 2 0.5)
          (make-from-mag-ang 3 0.4)))))

(deftest div-complex-should-produce-rectangular-as-sum-of-real-imag
  (is (=  (->Polar 3 0.4)
        (div
          (make-from-mag-ang 6 0.9)
          (make-from-mag-ang 2 0.5)))))

(deftest equ-should-return-true-for-identical-numbers
  (is (equ? (make-from-mag-ang 1 1) (make-from-mag-ang 1 1)))
  (is (equ? (make-from-real-imag 1 1) (make-from-real-imag 1 1))))

(deftest equ-should-return-false-for-identical-numbers
  (is (not (equ? (make-from-mag-ang 1 2) (make-from-mag-ang 3 4))))
  (is (not (equ? (make-from-real-imag 1 2) (make-from-real-imag 3 41)))))


(run-tests )
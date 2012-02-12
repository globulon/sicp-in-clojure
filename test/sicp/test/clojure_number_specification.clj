(ns sicp.test.clojure-number-specification
  (:use [sicp generic-arithmetic clojure-number])
  (:use clojure.test))

(deftest make-clojure-with-keyword-should-return-symbol
  (is (= :symb (make-clj :symb)))
  (is (nil? (tag-in (make-clj :symb)))))

(deftest make-clojure-with-symbol-should-return-symbol
  (is (= 'a (make-clj 'a)))
  (is (nil? (tag-in (make-clj ' a)))))

(deftest make-clojure-with-number-should-tagged-number
  (is (= {:value 1} (make-clj 1)))
  (is (= clj-type (tag-in (make-clj 1)))))

(deftest add-clojure-numbers-should-provide-result
  (is (= (make-clj 3) (add (make-clj 1) (make-clj 2)))))

(deftest mul-clojure-numbers-should-provide-result
  (is (= (make-clj 4) (add (make-clj 2) (make-clj 2)))))

(deftest sub-clojure-numbers-should-provide-result
  (is (= (make-clj 0) (sub (make-clj 2) (make-clj 2)))))

(deftest div-clojure-numbers-should-provide-result
  (is (= (make-clj 2) (sub (make-clj 4) (make-clj 2)))))

(deftest equ-should-return-true-on-numbers-equality
  (is (equ? (make-clj 1) (make-clj 1))))

(deftest equ-should-return-false-on-numbers-equality
  (is (not (equ? (make-clj 1) (make-clj 2)))))


;(run-tests)
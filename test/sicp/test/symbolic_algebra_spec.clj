(ns sicp.test.symbolic-algebra-spec
  (:use clojure.test)
  (:use sicp.symbolic-algebra))

(deftest variable?-shouold-return-true-for-symbol
  (is (variable? 'a)))

(deftest variable?-shouold-return-false-not-for-symbol
  (is (not (variable? 1))))

(deftest make-poly-should-setup-variable
  (is (= 'x (variable (make-poly 'x [1 2 3])))))

(deftest make-poly-should-setup-terms
  (is (= [1 2 3] (term-list (make-poly 'x [1 2 3])))))

(deftest same-variable?-should-return-true-one-match
  (is (same-variable? (make-poly 'x []) (make-poly 'x []))))

(deftest same-variable?-should-return-false-one-mismmatch
  (is (not (same-variable? (make-poly 'x []) (make-poly 'y [])))))

(run-tests)
(ns sicp.test.data-symbol-spec
  (:use sicp.data-symbol)
  (:use clojure.test))

(deftest sum?-should-recognize-input-expression
  (is (= true (sum? '(+ x 3))))
  (is (not= true (sum? '(* x 3)))))

(deftest product?-should-recognize-input-expression
   (is (= true (product? '(* x 3))))
   (is (not= true (product? '(+ x 3)))))

(deftest exponentiation?-should-identify-operation
  (is (= true (exponentiation? '( ** x 3))))
  (is (= false (exponentiation? '( * x 3)))))


(deftest make-product-with-simple-arguments-should-produce-expression
  (is (= '(* x 3) (make-product 'x 3))))

(deftest make-product-with-vector-arguments-should-produce-expression
  (is (= '(* x y z) (make-product '(x y z)))))

(deftest make-sum-with-simple-arguments-should-produce-expression
  (is (= '(+ x 3) (make-sum 'x 3))))

(deftest make-sum-with-simple-arguments-should-produce-expression
  (is (= '(+ x y z) (make-sum '(x y z)))))

(deftest make-sum-with-one-zero-argument-should-produce-symbol
  (is (= 'x (make-sum 'x 0))))

(deftest make-sum-with-two-zero-argument-should-produce-operation-symbol
  (is (= '(+ x y) (make-sum '(x 0 y 0)))))

(deftest make-sum-with-two-numbers-should-produce-number
  (is (= 3 (make-sum '(1 2)))))

(deftest deriv-of-known-expression-should-provide-result
  (is (=  1 (deriv '(+ x 3) 'x))))

(deftest base-from-exponentiation-should-match
  (is (= 'x (base (make-exponentiation 'x 3)))))

(deftest exponent-from-exponentiation-should-match
  (is (= 3 (exponent (make-exponentiation 'x 3)))))


(deftest deriv-composed-expression-should-provide-exercise-result
  (is (= '(+ (* x y) (* y (+ x 3))) (deriv '(* x y (+ x 3)) 'x))))

(run-tests)


;(deftest deriv-from-exponentiation-should-match
;  (is (= '(* 7 (** x 6)) (deriv (list '** x 7)))))

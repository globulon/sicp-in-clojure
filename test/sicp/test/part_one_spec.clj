(ns sicp.test.part-one-spec 
  (:use [sicp.part-one])
  (:use [clojure.test]))

(deftest test-eleven-with-number-lower-than-three-should-return-same
	(is (= 0 (test-eleven-recursive 0)))
	(is (= 1 (test-eleven-recursive 1)))
	(is (= 2 (test-eleven-recursive 2))))

(deftest test-eleven-with-number-greater-than-three-should-return-estimate
	(is (=  4 (test-eleven-recursive 3)))
	(is (= 11 (test-eleven-recursive 4)))
	(is (= 25 (test-eleven-recursive 5))))

(deftest test-eleven-iterative-with-number-lower-than-three-should-return-same
	(is (= 0 (test-eleven-iterative 0)))
	(is (= 1 (test-eleven-iterative 1)))
	(is (= 2 (test-eleven-iterative 2))))

(deftest test-eleven-iterative-with-number-greater-than-three-should-return-estimate
	(is (=  4 (test-eleven-iterative 3)))
	(is (= 11 (test-eleven-iterative 4)))
	(is (= 25 (test-eleven-iterative 5))))	

(deftest pascal-coef-on-row-zero-should-be-one
	(is (= 1 (pascal-coef 0 0))))

(deftest pascal-coef-on-row-one-should-be-one
	(is (= 1 (pascal-coef 1 1))))

(deftest pascal-coef-on-row-one-at-zero-should-be-one
	(is (= 1 (pascal-coef 1 0))))


(deftest pascal-coef-on-the-fly-should-mtch-estimates
	(is (= 2 (pascal-coef 2 1)))
	(is (= 3 (pascal-coef 3 1)))
	(is (= 3 (pascal-coef 3 2)))
	(is (= 4 (pascal-coef 4 1)))
	(is (= 6 (pascal-coef 4 2)))
	(is (= 4 (pascal-coef 4 3))))
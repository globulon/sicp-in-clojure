(ns sicp.test.part-two-intervals-spec
	(:use sicp.part-two-intervals)
	(:use clojure.test))

(deftest upper-bopund-of-interval-should-be-bound
	(is (= 5 (lower-bound (make-interval 5 10)))))

(deftest lower-bopund-of-interval-should-be-bound
	(is (= 10 (upper-bound (make-interval 5 10)))))

(deftest add-interval-should-sum-intervals
	(let [
		first-interval (make-interval 5 10)
		second-interval (make-interval 7 11)
		result (add-intervals first-interval second-interval)]
		(is (= 12 (lower-bound result)))
		(is (= 21 (upper-bound result)))))

(deftest mult-interval-should-select-min-max-products
	(let [
		first-interval (make-interval 5 10)
		second-interval (make-interval 7 11)
		result (mul-intervals first-interval second-interval)]
		(is (= 35 (lower-bound result)))
		(is (= 110 (upper-bound result)))))

(deftest div-interval-should-select-min-max-products
	(let [
		first-interval (make-interval 5 10)
		second-interval (make-interval 2 4)
		result (div-intervals first-interval second-interval)]
		(is (= 1.25 (lower-bound result)))
		(is (= 5.0 (upper-bound result)))))

(deftest sub-intervals-should-extend-bounds
	(let [
		first-interval (make-interval 5 10)
		second-interval (make-interval 2 4)
		result (sub-intervals first-interval second-interval)]
		(is (= 8 (upper-bound result)))
		(is (= 1 (lower-bound result)))))

(deftest witdth-for-interval
	(is (= 5.0 (width-interval (make-interval 5 15)))))

(deftest center-should-be-found-using-constructor-definition
	(is (= 5.0 (center (make-center-width 5 2)))))

(deftest percent-should-ne-found-using-constructor-definition
	(is (= 5.0 (center (make-percent 5 20))))
	(is (= 4.0 (lower-bound (make-percent 5 20))))
	(is (= 6.0 (upper-bound (make-percent 5 20))))
	(is (= 20.0 (percent (make-percent 5 20)))))



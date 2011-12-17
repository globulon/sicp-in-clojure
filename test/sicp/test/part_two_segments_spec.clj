(ns sicp.test.part-two-segments-spec
	(:use [ sicp.part-two-segments])
	(:use [clojure.test]))


(deftest x-point-of-made-point-should-be-bound
	(is (= 1 (x-point (make-point 1 2)))))

(deftest y-point-of-made-point-should-be-bound
	(is (= 2 (y-point (make-point 1 2)))))	

(deftest start-segment-of-segment-should-be-bound
	(is (= 
		(make-point 1 2)
		(start-segment 
			(make-segment 
				(make-point 1 2) 
				(make-point 2 3))))))

(deftest length-of-known-segment-should-match
	(is (= 5.0 (length-segment (make-segment (make-point 0 0) (make-point 3 4))))))				

(deftest end-segment-of-segment-should-be-bound
	(is (= 
		(make-point 2 3)
		(end-segment 
			(make-segment 
				(make-point 1 2) 
				(make-point 2 3))))))	
				
(deftest witdth-retangle-should-be-bound
	(let [
		width (make-segment (make-point 0 0) (make-point 1 0))
		length (make-segment (make-point 0 0) (make-point 0 1))]
	(is (= width (width-rectangle (make-rectangle width length))))))

(deftest length-retangle-should-be-bound
	(let [
		width (make-segment (make-point 0 0) (make-point 1 0))
		length (make-segment (make-point 0 0) (make-point 0 1))]
	(is (= length (length-rectangle (make-rectangle width length))))))

(deftest area-of-known-rectangle-should-match
	(let [
		width (make-segment (make-point 0 0) (make-point 1 0))
		length (make-segment (make-point 0 0) (make-point 0 1))]
		(is (= 1.0 (area-rectangle (make-rectangle width length))))))

(deftest perimeter-rectangle-should-match
	(let [
		width (make-segment (make-point 0 0) (make-point 1 0))
		length (make-segment (make-point 0 0) (make-point 0 1))]
		(is (= 4.0 (perimeter-rectangle (make-rectangle width length))))))

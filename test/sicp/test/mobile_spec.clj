(ns sicp.test.mobile-spec
	(:use sicp.mobile)
	(:use clojure.test))

(deftest make-branch-should-bind-items
	(is (= 10.0 (branch-length (make-branch 10.0 50.0))))
	(is (= 50.0 (branch-structure (make-branch 10.0 50.0)))))

(deftest make-mobile-should-bind-both-branches
	(let [l (make-branch 1 2) r (make-branch 3 4)]
		(is (= l  (left-branch (make-mobile l r))))
		(is (= r  (right-branch (make-mobile l r))))))

(deftest torque-on-branch-with-weight-should-match
	(is (= 10.0 (torque-on (make-branch 1 10.0)))))

(deftest torque-on-branch-with-mobile-should-match
	(is (= 3.0 
		(torque-on 
			(make-branch 1 
				(make-mobile 
					(make-branch 2 1.0)
					(make-branch 2 2.0)))))))

(deftest torque-on-branch-with-complex-mobile-should-match
	(is (= 3.5
		(torque-on 
			(make-branch 1 
				(make-mobile 
					(make-branch 2 1.0)
					(make-branch 2 
						(make-mobile 
							(make-branch 4 0.5)
							(make-branch 2 2)))))))))


(deftest total-weight-of-given-mobile-should-match
	(is (= 
		100.0 
		(total-weight-mobile 
			(make-mobile 
				(make-branch 1 10.0)
				(make-branch 1 
					(make-mobile 
						(make-branch 1 
							(make-mobile 
								(make-branch 1 30.0)
								(make-branch 1 40.0)))
						(make-branch 1 20.0))))))))

;; A test mobile: 
 ;; Level 
 ;; ----- 
 ;; 3                   4  |    8                                      
 ;;              +---------+--------+ 2                        
 ;; 2         3  |  9                                        
 ;;        +-----+----+ 1                                    
 ;; 1    1 | 2                                       
 ;;    +---+---+                             
 ;;    2       1                             


(deftest is-balance-should-be-true-for-given-mobile
	(is (= true 
		(balanced?
			(make-mobile 
				(make-branch 4
					(make-mobile 
						(make-branch 3
							(make-mobile
								(make-branch 1 2.0)
								(make-branch 2 1.0)))
						(make-branch 9 1.0)))
				(make-branch 8 2.0))))))

(deftest is-balance-should-be-false-for-given-mobile
	(is (= false 
		(balanced?
			(make-mobile 
				(make-branch 4
					(make-mobile 
						(make-branch 3
							(make-mobile
								(make-branch 1 2.0)
								(make-branch 2 1.0)))
						(make-branch 9 1.0)))
				(make-branch 9 2.0))))))				
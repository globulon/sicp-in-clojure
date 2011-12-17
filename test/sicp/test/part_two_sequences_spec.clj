(ns sicp.test.part-two-sequences-spec
	(:use [sicp.part-two-sequences])
	(:use [clojure.test]))


(deftest last-pair-from-created-list-should-match
	(is (= '(34) (last-pair '(23 72 149 34)))))

(deftest my-reverse-should-reverse-items-in-list
	(is (= '(3 2 1) (my-reverse '(1 2 3)))))

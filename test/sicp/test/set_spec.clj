(ns sicp.test.set-spec
  (:use sicp.set)
  (:use clojure.test))

(deftest element-of-set?-should-return-true-when-elemen-in-set
  (is (= true (element-of-set? 1 '(1 2 3)))))

(deftest element-of-set?-should-return-false-when-no-element-in-set
  (is (= false (element-of-set? 4 '(1 2 3)))))

(deftest adjoin-one-element-with-set-should-extend-set
  (is (= (list 4 1 2 3) (adjoin-set 4 '(1 2 3)))))

(deftest intersection-set-with-set-containing-elements-in-common-should-provide-list
  (is (= '(2 3) (intersection-set '(1 2 3) '(2 3 4)))))

(deftest intersection-set-with-set-containing-no-elements-in-common-should-be-null
  (is (= '() (intersection-set '(1 2 3) '(4 5 6)))))

(deftest union-set-with-set-containing-elements-in-common-should-not-double-items
  (is (= '(1 2 3 4) (union-set '(1 2 3) '(2 3 4)))))

(deftest union-set-with-set-containing-no-elements-in-common-should-append
  (is (= '(1 2 3 4 5 6) (union-set '(1 2 3) '(4 5 6)))))

(deftest element-of-ordered-set?-with-existing-element-should-return-true
  (is (= true (element-of-set? 2 '(1 2 3)))))

(deftest element-of-ordered-set?-with-no-existing-element-should-return-false
  (is (= false (element-of-set? 4 '(1 2 3)))))

(deftest intersection-of-ordered-set-with-set-containing-elements-in-common-should-provide-list
  (is (= '(2 3) (intersection-of-ordered-set '(1 2 3) '(2 3 4)))))

(deftest intersection-of-ordered-set-with-set-containing-no-elements-in-common-should-be-null
  (is (= '() (intersection-of-ordered-set '(1 2 3) '(4 5 6)))))

(deftest adjoin-one-element-with-ordered-set-should-extend-set
  (is (= '(1 2 3 4) (adjoin-ordered-set 4 '(1 2 3)))))


(deftest union-oredered-set-with-set-containing-elements-in-common-should-not-double-items
  (is (= '(1 2 3 4) (union-ordered-set '(1 2 3) '(2 3 4)))))

(deftest union-oredered-set-with-set-containing-no-elements-in-common-should-append
  (is (= '(1 2 3 4 5 6) (union-ordered-set '(1 2 3) '(4 5 6)))))

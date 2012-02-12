(ns sicp.test.all-number-operations-spec
  ^{:doc "supposedly complete exercise 2.83 and 2.84"}
  (:use clojure.test)
  (:use [sicp
         generic-arithmetic
         clojure-number
         rational
         complex
         all-number-operations]))

(deftest raise-from-integer-shouldraise-to-rational
  (is (= rat-type (tag-in (raise (make-clj 1))))))

;(deftest raise-from-rational-should-raise-to-complex
;  (is (= complex-type (tag-in (raise (make-rat 1 2))))))
;

;(run-tests )
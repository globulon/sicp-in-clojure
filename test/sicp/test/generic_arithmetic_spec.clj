(ns sicp.test.generic-arithmetic-spec
  (:use sicp.generic-arithmetic)
  (:use sicp.rational)
  (:use clojure.test))

(deftest tagged-data-should-be-identifiable
  (is (= "test"(tag-in (tagged (make-rat 1 2) "test" )))))


(ns sicp.test.insatiable-spec
  (:use sicp.insatiable)
  (:use sicp.insatiable-1)
  (:use sicp.insatiable-2)
  (:use clojure.test))

(def files [(->PersonalFile1) (->PersonalFile2)])

(deftest get-record-should-route-correct-answer
  (is (= 'name-1 (.employee-id (first (get-record {:employee-id 'name-1} (->PersonalFile1))))))
  (is (= 'name-2 (.employee-id (first (get-record {:employee-id 'name-2} (->PersonalFile2)))))))

(deftest find-employee-record-from-should-bring-back-employee
  (is (= 'name-1 (.employee-id (first (find-employee-record {:employee-id 'name-1} files))))))


(run-tests )





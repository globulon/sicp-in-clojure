(ns sicp.insatiable-1
  (:use sicp.insatiable))
;;May cover exercise 2.74

(defrecord EmployeeRecord1 [identifier employee salary]
  EmployeeRecord
  (employee-id [this] (:employee this))
  (salary-in [this] (:salary this)))


(defn- records [] [(->EmployeeRecord1 1 'name-1 1000)])

;;May cover exercise 2.74
(defrecord PersonalFile1 []
  PersonalFile
  (division[this] 'division1)
  (record-of[this employee]
    (filter
      (fn [item]
        (= (.employee-id item) (:employee-id employee))) (records) )))




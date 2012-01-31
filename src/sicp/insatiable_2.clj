(ns sicp.insatiable-2
  (:use sicp.insatiable))

(defrecord EmployeeRecord2 [identifier employee salary]
  EmployeeRecord
  (employee-id [this] (:employee this))
  (salary-in [this] (:salary this)))

(defn- records [] [(->EmployeeRecord2 2 'name-2 2000)])

;;May cover exercise 2.74
(defrecord PersonalFile2 []
  PersonalFile
  (division[this] 'division2)
  (record-of[this employee]
    (filter
      (fn [item]
        (= (.employee-id item) (:employee-id employee))) (records) )))


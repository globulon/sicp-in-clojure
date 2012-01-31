(ns sicp.insatiable)
;;May cover exercise 2.74
(defprotocol PersonalFile
  (division[this])
  (record-of[this employee]))

(defprotocol EmployeeRecord
  (employee-id [this])
  (salary-in [this]))

(defn get-record [employee file]
  (record-of file employee))

(defn get-salary [record]
  (salary-in record))

(defn find-employee-record[employee in-records]
  (flatten (map (fn [record] (get-record employee record)) in-records)))
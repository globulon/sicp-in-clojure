(ns sicp.test.records-spec
  (:use sicp.records)
  (:use sicp.tree)
  (:use clojure.test))

(def a-tree (list->tree
              (list
                (make-record 1 1)
                (make-record 2 2)
                (make-record 3 3)
                (make-record 4 4))))

(deftest key-record-should-restore-key
  (is (= 2 (key-record (make-record 2 2)))))

(deftest data-should-restore-data
  (is (= 3 (data (make-record 2 3)))))


(deftest check-test-tree-structure
  (is (= (make-record 2 2) (entry a-tree ))))

(deftest lookup-should-find-existing-key-in-tree
    (is (= 2 (lookup 2 a-tree))))

(deftest lookup-should-find-existing-key-in-tree
    (is (= false (lookup 5 a-tree))))


(run-tests )

(ns sicp.test.tree-spec
  (:use sicp.tree)
  (:use clojure.test))


(deftest tree->list-from-list->tree-should-return-same-as-input
  (is (= '(1 2 3) (tree->list (list->tree '(1 2 3))))))

(deftest union-tree-should-unify-tree-data
  (let [t1 (list->tree '(1 2 3)) t2 (list->tree '(2 3 4))]
    (is (= '(1 2 3 4)
          (tree->list (union-tree t1 t2))))))


(deftest intersection-tree-should-find-common-tree-data
  (let [t1 (list->tree '(1 2 3)) t2 (list->tree '(2 3 4))]
    (is (= '(2 3)
          (tree->list (intersection-tree t1 t2))))))

(run-tests )


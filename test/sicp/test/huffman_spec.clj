(ns sicp.test.huffman-spec
  (:use sicp.huffman)
  (:use clojure.test))

(deftest symbol-leaf-should-find-symbol-in-leaf
  (is (= "A" (symbol-leaf (make-leaf "A" 1)))))

(deftest weight-leaf-should-find-weight-in-leaf
  (is (= 1 (weight-leaf (make-leaf "A" 1)))))

(deftest leaf-should-be-true-for-leaf
  (is (= true (leaf? (make-leaf "A" 1)))))

(deftest leaf-should-be-false-for-not-leaf
  (is (= false (leaf? [1 2 3]))))

(deftest left-branch-should-match-created-branch
  (let [left  (make-leaf "A" 1)
        right (make-leaf "B" 1)]
    (is (= left (left-branch (make-code-tree left right))))))

(deftest right-branch-should-match-created-branch
  (let [left  (make-leaf "A" 1)
        right (make-leaf "B" 1)]
    (is (= right (right-branch (make-code-tree left right))))))

(deftest symbol-in-leaf-should-be-symbol
  (is (= ["A"] (symbols (make-leaf "A" 1)))))

(deftest symbol-in-tree-node-should-be-concat-symbols
  (is (= ["A" "B"]
        (symbols
          (make-code-tree
            (make-leaf "A" 1)
            (make-leaf "B" 1))))))

(deftest weight-of-leaf-should-match-leaf-weight
  (is (= 2 (weight (make-leaf "D" 2)))))

(deftest weight-of-tree-should-match-sum-of-weight
  (is (= 2
        (weight
          (make-code-tree
            (make-leaf "A" 1)
            (make-leaf "B" 1))))))

(def sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest decode-sample-message-with-sample-tree-should-give-message
  (is (= true (vector? (decode sample-message sample-tree ))))
  (is (= '(A D A B B C A) (decode sample-message sample-tree ))))

(deftest encode-decoded-message-should-produce-seed-message
  (is (= sample-message
        (encode
          (decode sample-message sample-tree) sample-tree))))

(def song-symbols ['(A 2) '(BOOM 1) '(GET 2) '(JOB 2) '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1)])
(def song (vec '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)))

(deftest encode-song
  (let [song-tree (make-huffman-tree song-symbols )]
    (is (= 86 (count (encode song song-tree))))
    (is (= song
          (decode
            (encode song song-tree) song-tree)))))
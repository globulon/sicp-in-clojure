(ns sicp.tree
  (:use [sicp.set :only (intersection-of-ordered-set union-ordered-set)]))

(defn entry [tree]
  (first tree))

(defn left-branch [tree]
  (second tree))

(defn right-branch [tree]
  (second (rest tree)))

(defn make-tree [value left right]
  (list value left right))

(defn make-leaf [from-data]
  (make-tree from-data '() '()))


(defn element-of-set? [x tree]
  (cond
    (empty? tree) false
    (= x (entry tree)) true
    (< x (entry tree)) (element-of-set? x (left-branch tree))
    (> x (entry tree)) (element-of-set? x (right-branch tree))))

(defn adjoin-set [x tree]
  (cond
    (empty? tree) (list x '() '())
    (= x (entry tree)) tree
    (< x (entry tree)) (make-tree
                         (entry tree)
                         (adjoin-set x (left-branch tree))
                         (right-branch tree))
    (> x (entry tree)) (make-tree
                         (entry tree)
                         (left-branch tree)
                         (adjoin-set x (right-branch tree)))))

(defrecord TreePart [balanced unbalanced])

(defn- partial-tree [items n]
  (if (= 0 n)
    (TreePart. '() items)
    (let [
           left-size (quot (dec n) 2)
           left-result (partial-tree items left-size)
           left-tree (:balanced left-result)
           not-left-tree (:unbalanced left-result)
           this-entry (first not-left-tree)
           right-size (- n (inc left-size))
           right-result (partial-tree (rest not-left-tree) right-size)
           right-tree (:balanced right-result)
           remaining (:unbalanced right-result)]
      (TreePart. (make-tree this-entry left-tree right-tree) remaining))))

(defn list->tree [items]
  (:balanced (partial-tree items (count items))))

(defn tree->list [data]
  (letfn [(copy-of [tree to-list]
            (if (empty? tree)
              to-list
              (recur
                (left-branch tree)
                (cons
                  (entry tree)
                  (copy-of (right-branch tree) to-list)))))]
    (copy-of data '())))

(defn operate-on-sets [t1 t2 with-operator]
  (let [s1 (tree->list t1) s2 (tree->list t2)]
          (list->tree (with-operator s1 s2))))

(defn union-tree [t1 t2]
  (operate-on-sets t1 t2 union-ordered-set))

(defn intersection-tree [t1 t2]
  (operate-on-sets t1 t2 intersection-of-ordered-set))



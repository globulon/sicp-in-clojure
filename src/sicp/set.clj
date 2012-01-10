(ns sicp.set)

(defn element-of-set? [x s]
  (cond
    (empty? s) false
    (= x (first s)) true
    :else (element-of-set? x (rest s))))

(defn adjoin-set [x s]
  (if (element-of-set? x s)
    s
    (cons x s)))

(defn intersection-set [s1 s2]
  (cond
    (or (empty? s1) (empty? s2)) '()
    (element-of-set? (first s1) s2) (cons (first s1) (intersection-set (rest s1) s2))
    :else (intersection-set (rest s1) s2)))

(defn union-set [s1 s2]
  (cond
    (empty? s1) s2
    (element-of-set? (first s1) s2) (union-set (rest s1) s2)
    :else (cons (first s1) (union-set (rest s1) s2))))

(defn element-of-ordered-set? [x set]
  (cond
    (empty? set) '()
    (= x (first set)) true
    (< x (first set)) false
    :else (element-of-set? x (rest set))))

(defn intersection-of-ordered-set [s1 s2]
  (if (or (empty? s1) (empty? s2))
    '()
    (let [x1 (first s1) x2 (first s2)]
      (cond
        (= x1 x2) (cons x1
                    (intersection-of-ordered-set
                      (rest s1) (rest s2)))
        (< x1 x2) (recur (rest s1) s2)
        (> x1 x2) (recur s1 (rest s2))))))

(defn adjoin-ordered-set [x set]
  (cond 
    (empty? set) (list x)
    (= x (first set)) set
    (< x (first set)) (cons x set)
    (> x (first set)) (cons
                        (first set)
                        (adjoin-ordered-set x (rest set)))))

(defn union-ordered-set [s1 s2]
  (cond 
    (empty? s1) s2
    (empty? s2) s1
    ( = (first s1) (first  s2))
    (cons (first s1) (union-ordered-set (rest s1) (rest s2)))
    ( < (first s1) (first  s2))
    (cons (first s1) (union-ordered-set (rest s1) s2))
    ( > (first s1) (first  s2))
    (cons (first s2) (union-ordered-set s1 (rest s2)))))


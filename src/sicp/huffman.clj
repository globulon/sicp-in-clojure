(ns sicp.huffman)

(defn make-leaf [symbl weight]
  [:leaf symbl weight])


(defn symbol-pair [pair]
  (first pair))

(defn value-pair [pair]
  (second pair))


(defn make-leaf-from-pair [pair]
  (make-leaf (symbol-pair pair) (value-pair pair)))

(defn symbol-leaf [from-leaf]
  (second from-leaf))

(defn weight-leaf [from-leaf]
  (peek from-leaf))

(defn leaf? [item]
  (= :leaf (first item)))

(defn symbols [tree-node]
  (cond
    (empty? tree-node) []
    (leaf? tree-node) [(symbol-leaf tree-node)]
    :else (nth tree-node 2)))

(defn weight [tree-node]
  (cond
    (empty? tree-node) 0
    (leaf? tree-node) (weight-leaf tree-node)
    :else (nth tree-node 3)))

(defn make-code-tree [left right]
  [left
   right
   (concat (symbols left) (symbols right))
   (+ (weight left) (weight right))])

(defn left-branch [from-tree]
  (first from-tree))

(defn right-branch [from-tree]
  (second from-tree))

(defn- next-branch-from [bits from-tree]
  {:pre [(not (empty? bits)) (not (empty? from-tree))]}
  (if (= 0 (first bits))
    (left-branch from-tree)
    (right-branch from-tree)))

(defn decode [bits from-tree]
  (letfn [(decode-iter [remaining-bits current-branch message]
            (if (empty? remaining-bits)
              message
              (let [next-branch (next-branch-from remaining-bits current-branch)]
                (if (leaf? next-branch)
                  (recur (rest remaining-bits) from-tree (conj message (symbol-leaf next-branch)))
                  (recur (rest remaining-bits) next-branch message)))))]
    (decode-iter bits from-tree [])))

(defn symbol-in-tree? [value from-tree]
  (if
    (empty? from-tree) false
    (= [value] (filter #(= value %) (symbols from-tree)))))


(defn encode-symbol [value from-tree]
  (if (not (symbol-in-tree? value from-tree))
    (throw (Exception. "value not found in tree"))
    (letfn [(encode-symbol-iter [branch encoding]
              (let [left (left-branch branch) right (right-branch branch)]
                (cond
                  (leaf? branch) encoding
                  (symbol-in-tree? value left) (recur
                                                 (left-branch branch)
                                                 (conj encoding 0))
                  (symbol-in-tree? value right) (recur
                                                  (right-branch branch)
                                                  (conj encoding 1)))))]
      (encode-symbol-iter from-tree []))))

(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-symbol (first message) tree)
      (encode (rest message) tree))))


(defn- filter-pairs-versus [pivot comparing]
  (let [symbl (symbol-pair pivot) value (value-pair pivot)]
    (fn [pair]
      (and (comparing (second pair) value) (not= symbl (first pair))))))


(defn- greater-than [pivot] (filter-pairs-versus pivot >=))

(defn- lower-than [pivot] (filter-pairs-versus pivot <))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    []
    (let [car (first pairs)]
      (vec (concat
             (conj (make-leaf-set (filter (lower-than car) pairs)) (make-leaf-from-pair car))
             (make-leaf-set (filter (greater-than car) pairs)))))))

(defn- tail-from [n sequence]
  (vec (second (split-at n sequence))))

(defn successive-merge [nodes]
  (let [size (count nodes)]
    (cond
      (= 1 size) nodes
      (= 2 size) (make-code-tree
                   (second nodes)
                   (first nodes))
      :else
      (let [car (first nodes) cadr (second nodes) caddr (nth nodes 2)]
        (if (>= (weight cadr) (weight car))
          (recur (cons (make-code-tree cadr car) (tail-from 2 nodes)))
          (recur (cons car (cons (make-code-tree caddr cadr) (tail-from 3 nodes)))))))))

(defn make-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

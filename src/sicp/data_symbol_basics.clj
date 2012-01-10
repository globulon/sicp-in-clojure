(ns sicp.data-symbol-basics)

(defn memq [item symbols]
  (cond
    (empty? symbols) false
    (= item (first symbols)) symbols
    :else (recur item (rest symbols))))

(defn equal? [items-one items-two]
  (cond
    (and (seq? items-one) (seq? items-two))
    (if (and (empty? items-one) (empty? items-two))
      true
      (and
        (equal? (first items-one) (first items-two))
        (recur (rest items-one) (rest items-two))))
    (and (not (seq? items-one)) (not (seq? items-two)))
    (= items-one items-two)
    :else false))

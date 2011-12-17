(ns sicp.data-symbol)

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

(defn variable? [expression]
  (symbol? expression))

(defn sum? [expression]
  (= '+ (first expression)))

(defn product? [expression]
  (= '* (first expression)))

(defn same-variable? [expression variable]
  (and (variable? expression) (= expression variable)))

(defn make-sum [x y]
  (cond
    (= 0 x) y
    (= 0 y) x
    (and (number? x) (number? y)) (+ x y)
    :else (list '+ x y)))

(defn addend [expression]
  {:pre [(= 3 (count expression))]}
  (second expression))

(defn augend [expression]
  {:pre [(= 3 (count expression))]}
  (second (rest expression)))

(defn make-product [x y]
  (cond
    (= 0 x) 0
    (= 0 y) 0
    (= 1 x) y
    (= 1 y) x
    (and (number? x) (number? y)) (* x y)
    :else ('* x y)))

(defn multiplier [expression]
  {:pre [(= 3 (count expression))]}
  (second expression))

(defn multiplicand [expression]
  {:pre [(= 3 (count expression))]}
  (second (rest expression)))

(defn deriv [expression variable]
  (cond
    (number? expression)
     0
    (variable? expression)
    (if (same-variable? expression variable) 1 0)
    (sum? expression)
    (make-sum
      (deriv (addend expression) variable)
      (deriv (augend expression) variable))
    (product? expression)
    (make-sum
      (make-product
        (multiplier expression)
        (deriv (multiplicand expression) variable))
      (make-product
        (deriv (multiplier expression) variable)
        (multiplicand expression)))
    :else (println "Invalid expression")))
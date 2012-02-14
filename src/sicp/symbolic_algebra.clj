(ns sicp.symbolic-algebra
  ^{:doc "section 2.5.3 on symbolic algebra"}
  (:use sicp.generic-arithmetic))

(def poly-type ::polynomial)

(defn- tag [poly]
  (tagged poly poly-type))

(defn variable [from-poly]
  (first from-poly))

(defn term-list [from-poly]
  (second from-poly))

(defn make-poly [var term-list]
  (tag [var term-list]))

(defn variable? [expression]
  (symbol? expression))

(defn same-variable? [exp1 exp2]
  (= (variable exp1) (variable exp2)))


(defn order [of-ter])
(defn coeff [of-ter])
(defn empty-terms? [terms])
(defn make-term [order coeff])
(defn rest-term [of-terms])
(defn first-term [of-terms])
(defn adjoin-term [term terms])

(defn- add-terms [terms1 terms2]
  (cond
    (empty-terms? terms1) terms2
    (empty-terms? terms2) terms1
    :else (let
            [ t1 (first-term terms1) t2 (first-term terms2)
              order1 (order t1) order2 (order t2)]
            (cond
              (> order1 order2) (adjoin-term t1 (add-terms (rest-term t1) t2))
              (< order1 order2) (adjoin-term t2 (add-terms (rest-term t2) t1))
              :else (adjoin-term
                      (make-term (order t1) (add (coeff t1) (coeff t2)))
                      (add-terms (rest-term t1) (rest-term t2)))))))



(defn- mul-terms [terms1 terms2])

(defn- apply-operation [op p1 p2]
  (if (same-variable? p1 p2)
    (make-poly
      (variable p1)
      (op (term-list p1) (term-list p2)))
    (throw (Exception. "polynomials must have same variables"))))

(defn- add-poly [p1 p2] (apply-operation add-terms p1 p2))

(defn- mul-poly [p1 p2] (apply-operation mul-terms p1 p2))

(defmethod add [poly-type poly-type] [p1 p2]
  (add-poly p1 p2))

(defmethod mul [poly-type poly-type] [p1 p2]
  (mul-poly p1 p2))


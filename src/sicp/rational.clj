(ns sicp.rational
  (:use sicp.generic-arithmetic))

(def rat-type ::rational)

(defn- gcd [a b]
	(cond 
		(> b a) (recur b a)
		(= 0 b) a
		:else (recur b (- a b))))

(defn- abs [x]
	(if (< x 0) 
		(* -1 x)
		x))

(defn- sgn [numerator denominator] 
	(cond 
		(and (> numerator 0) (< denominator 0)) -1
		(and (< numerator 0) (> denominator 0)) -1
		:else 1))

(defn make-rat [numerator denominator] 
	(let [
		norm (gcd (abs numerator) (abs denominator))
		sign (sgn numerator denominator)	]
    (tagged [(/ numerator norm) (/ denominator norm)] rat-type)))

(defn numer [from-rat] 
	(first from-rat))

(defn denum [from-rat] 
	(second from-rat))

(defn- add-rat [num-one num-two]
	(make-rat
		(+ 
			(* (numer num-one) (denum num-two))
			(* (numer num-two) (denum num-one)))
		(* (denum num-one) (denum num-two))))

(defn- sub-rat [num-one num-two]
	(make-rat
		(- 
			(* (numer num-one) (denum num-two))
			(* (numer num-two) (denum num-one)))
		(* (denum num-one) (denum num-two))))		

(defn- mul-rat [num-one num-two]
	(make-rat
		(* (numer num-one) (numer num-two))
		(* (denum num-one) (denum num-two))))				

(defn- div-rat [num-one num-two]
	(make-rat
		(* (numer num-one) (denum num-two))
		(* (denum num-one) (numer num-two))))

(defn- equal? [num-one num-two]
	(= 
		(* (numer num-one) (denum num-two))
		(* (numer num-two) (denum num-one))))		

(defn- print-rat [from-number]
	(println 
		(str 
			(numer from-number) 
			"\\" 
			(denum from-number) "\n"))
	nil)

(defmethod add [rat-type rat-type] [x y]
  (add-rat x y))

(defmethod sub [rat-type rat-type] [x y]
  (sub-rat x y))

(defmethod mul [rat-type rat-type] [x y]
  (mul-rat x y))

(defmethod div [rat-type rat-type] [x y]
  (div-rat x y))

(defmethod
  ^{:doc "ex 2.78"}
  equ? [rat-type rat-type] [x y]
  (equal? x y))
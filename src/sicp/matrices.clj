(ns sicp.matrices
	(:use sicp.higher-order))

(defn product-dot [v w]
	(reduce + 0 (map * v w)))

(defn matrix-*-vector [m v]
	(map (fn [x] (product-dot x v)) m))

(defn transpose [m]
	(reduce-n conj [] m))	

(defn matrix-*-matrix [m n]
	(let [cols (transpose n)]
		(map (fn [m-line] (matrix-*-vector cols m-line)) m)))




		
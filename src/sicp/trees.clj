(ns sicp.trees)

(defn scale-tree-direct [tree factor]
	(cond 
		(not (seq? tree)) (* tree factor)
		(empty? tree) '()
		:else (cons 
			(scale-tree (first tree) factor)
			(scale-tree (rest tree) factor))))

(defn square-tree-direct [tree]
	(cond 
		(not (seq? tree)) (* tree tree)
		(empty? tree) '()
		:else (cons 
			(square-tree-direct (first tree))
			(square-tree-direct (rest tree)))))

(defn map-tree [proc tree]
	(letfn [
		(map-item [item]
			(cond 
				(not (seq? item)) (proc item)
				(empty? item) '()
				:else (map map-item item)))]
	(map map-item tree)))

(defn square-tree [tree]
	(map-tree #(* % %) tree))

(defn scale-tree [tree factor]
	(map-tree #(* factor %) tree))

(defn subsets [s]
	(if (empty? s)
		(list '())
		(let [cdr (subsets (rest s))]
			(concat cdr (map #(cons (first s) %) cdr)))))
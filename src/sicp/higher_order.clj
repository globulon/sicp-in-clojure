(ns sicp.higher-order)

(defn horner-evaluation [x coefficients]
	(letfn [
		(horner-partial-eval [higher-terms this-coeff]
			(+ (* higher-terms x) this-coeff))]
	(reduce horner-partial-eval 0 (reverse coefficients))))

(defn count-leaves [tree]
	(letfn [
		(adopt-strategy [for-item]
			(if (not (seq? for-item))
				1
				(count-leaves for-item)))]
	(reduce + 0 (map adopt-strategy tree))))

(defn- all-empty? [seqs]
	(reduce (fn [acc cur] (and acc (empty? cur))) true seqs))

(defn reduce-n [op from-zero seqs]
	(if (all-empty? seqs) 
		'()
		(cons 
			(reduce op from-zero (map first seqs))
			(reduce-n op from-zero (map rest seqs)))))




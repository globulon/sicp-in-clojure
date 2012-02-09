(ns sicp.coins)

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn except-first-denomination [coin-values]
	(rest coin-values))

(defn first-denomination [coin-values]
	(first coin-values))

(defn cc [amount coin-values]
	(cond 
		(= amount 0) 1
		(or (< amount 0) (empty? coin-values)) 0
		:else
			(+ 
				(cc 
					amount 
					(except-first-denomination coin-values))
				(cc 
					(- amount (first-denomination coin-values)) 
					coin-values))))


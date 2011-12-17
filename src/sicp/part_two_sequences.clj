(ns sicp.part-two-sequences
	(:use sicp.part-one))

(defn last-pair [from-list]
	(letfn [
		(last-pair-iter [current remaining-list]
			(if (empty? remaining-list)
				current
				(recur (list (first remaining-list)) (rest remaining-list))))]
		(last-pair-iter '() from-list)))

(defn my-reverse [from-list]
	(letfn [
		(my-reverse [accumulated remaining]
			(if (empty? remaining)
				accumulated
				(recur 
					(cons (first remaining) accumulated) 
					(rest remaining))))]
		(my-reverse '() from-list)))

;Not that could be achieved using a filter higher order function
(defn same-parity [number & others]
	(letfn [
		(parity-iter[parity? accumulated remaining]
			(cond 
				(empty? remaining) accumulated
				(parity? (first remaining)) (recur parity? (conj accumulated (first remaining)) (rest remaining)) 
				:else (recur parity? accumulated (rest remaining))))]
	(if (odd? number) 
		(parity-iter odd? [number] others)
		(parity-iter even? [number] others))))

(defn for-each [items proceed]
	(if (empty? items)
		nil
		(do 
			(proceed (first items) )
			(for-each (rest items) proceed))))

(defn count-leaves [items]
	(cond 
		(not (seq? items)) 1
		(empty? items) 0
		:else (+ 
			(count-leaves (first items))
			(count-leaves (rest items)))))

(defn deep-reverse [input-list]
	(letfn [
		(iter [accumulated current]
			(if (empty? current)
				accumulated
				(let [car (first current)]
					(if (seq? car)
						(recur (cons (deep-reverse car) accumulated) (rest current))
						(recur (cons car accumulated) (rest current))))))]
		(iter '() input-list)))


(defn append [list-one list-two]
	(letfn[
		(iter [current accumulated]
			(if (empty? current)
				accumulated
				(recur (rest current) (cons (first current) accumulated))))]
		(iter (reverse list-one) list-two)))

(defn fringe [items]
	(letfn [
		(iter [current accumulated]
			(cond 
				(empty? current) 
					accumulated
				(not (seq? (first current))) 
					(recur (rest current) (cons (first current) accumulated))
				:else
					(recur (rest current) (append (iter (first current) '()) accumulated))))]
		(reverse (iter items '()))))

(defn filtered [matching s]
	(cond 
		(empty? s) '()
		(matching (first s)) 
			(cons (first s) (filtered matching (rest s)))
		:else (filtered matching (rest s))))		

(defn accumulated [operation from-zero s]
	(if (empty? s) 
		from-zero 
		(operation 
			(first s) 
			(accumulated 
				operation from-zero (rest s)))))

(defn foldr [operation from-zero s]
  (accumulated operation from-zero s))

(defn append [seq1 seq2]
	(reduce conj (vec seq1) seq2))

(defn map-high[with-proc s]
	(reduce #(conj %1 (with-proc %2)) [] (vec s)))

(defn length[s]
	(reduce (fn [acc item] (inc acc)) 0 s))

(defn flatmap[op sequence]
	(reduce 
		concat '() (map op sequence)))

(defn prime-sum? [pair]
	(prime? (+ (first pair) (second pair))))

(defn combine-values [from to]
	(for [x (range from to) y (range from to)] 
		(list x y)))

(defn prime-pairs [from to]
	(map
		(fn [[a b]] (list (list a b) (+ a b)))
		(filter prime-sum? (combine-values 1 6))))

(defn remove-from [sequence item]
	(remove #(= item %) sequence))

(defn permutations [s]
  (if (empty? s)
    (list '())
    (flatmap
      (fn [x]
        (map
          (fn [y](cons x y))
          (permutations (remove-from s x))))
      s)))

(defn enumerate-interval [a b]
  (range a (inc b)))

(defn unique-pairs [n]
  (flatmap
    (fn [x]
      (map #(list x %) (enumerate-interval 1 x)))
    (enumerate-interval 1 n)))

(defn triples [n]
  (flatmap
    (fn [x]
      (map #(cons x %) (unique-pairs x)))
    (enumerate-interval 1 n)))

(defn sum-matches [ijk s]
  (= s (apply + ijk)))

(defn matching-triples [n s]
  (filter #(sum-matches % s) (triples n)))

(defn adjoin-position [new-row k other-queens]
	(conj other-queens [k new-row]))

(defn abs [x] (if (> x 0) x (- x)))

(defn unsafe? [[col row] [col0 row0]]
	(or (= row row0)
		(= 1 (abs (/ (- row row0) (- col col0))))))

(defn safe? [positions]
	(letfn [
		(check [remaining position]
			(cond 
				(empty? remaining) true
				(unsafe? (peek remaining) position) false
				:else (recur (pop remaining) position)))]
	(check (pop positions) (peek positions))))

(defn queens [board-size]
	(let [interval (vec (enumerate-interval 1 board-size))]
		(letfn [
			(queen-cols [k]
				(if (= 0 k)
					[[]]
					(vec (filter 
						(fn [positions] (safe? positions))
						(vec (flatmap 
							(fn [other-queens] 
								(vec (map 
									(fn [new-row] (adjoin-position new-row k other-queens))
									interval ))) 
							(queen-cols (dec k))))))))]
		(queen-cols board-size))))


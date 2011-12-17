(ns sicp.part-one)

(defn test-eleven-recursive [n]
	(if (< n 3)
		n
		(+ 
			(test-eleven-recursive (dec n))
			(* 2 (test-eleven-recursive (- n 2)))
			(* 3 (test-eleven-recursive (- n 3))))))

(defn test-eleven-iterative [n]
	(letfn [
		(iteration [a b c counter]
			(if (= 0 counter)
				c
				(iteration
					(+ a (* 2 b) (* 3 c)) 
					a  
					b 
					(dec counter))))]
		(iteration 2 1 0 n)))

(defn pascal-coef [m n]
	(cond 
		(= 0 m) 1
		(= 0 n) 1
		(= n m) 1
		:else (+ 
			(pascal-coef (dec m) (dec n)) 
			(pascal-coef (dec m) n))))


(defn fast-expt-iterative [b n]
	(if (= 0 n ) 
		1
		(letfn [
				(iterate-on [accumulated b-square power]
					(if (= power 0) 
						accumulated
						(recur (* accumulated b-square) b-square (dec power))))]
			(let [b-square (* b b)]
				(if (even? n)
					(iterate-on 1 b-square (/ n 2))
					(iterate-on b b-square (/ (dec n) 2)))))))


(defn twice [n]
	(* n 2))

(defn halve [n]
	(if (= 1 n)
		0 
		(/ n 2)))	

(defn multiply [a b]
	(cond 
		(= 0 b) 0
		(even? b) (twice (multiply a (halve b)))
		:else (+ a (multiply a (dec b)))))

(defn multiply-iter [a b]
	(letfn [
		(iterate-on [accumulator divider]
			(println accumulator " " divider)
			(cond
				(= 0 divider) 0
				(= 1 divider) accumulator
				(even? divider) (recur (twice accumulator) (halve divider))
				:else (recur (+ a (twice accumulator)) (halve (dec divider)))))]
		(iterate-on a b)))

(defn square [n] (* n n))

(defn find-smallest-divisor [n step]
	(cond
		(> (square step) n) n
		(= (rem n step) 0) step
		:else (recur n (inc step))))

(defn smallest-divisor [n]
	(find-smallest-divisor n 2))

(defn prime? [n]
	(= n (smallest-divisor n)))

(defn three-first-prime-greater-than [n]
	(letfn [
		(find-primes [from-value already-found]
			(cond 
				(= 3 (count already-found)) already-found
				(prime? from-value) (recur (inc from-value) (cons from-value already-found))
				:else (recur (inc from-value) already-found)))]
		(find-primes n [])))


(defn expmod-iter [number exp mod]
	(letfn [
		(iter-on [accumulator operand pow step]
			(cond 
				(= 1 pow)  (* operand accumulator)
				(even? pow) (recur (square accumulator) operand (/ pow 2) (inc step))
				:else (recur accumulator (* operand accumulator) (dec pow) (inc step))))]
		(let [result (iter-on (* 1N number) 1N exp 1)]
			(rem result mod))))

(defn expmod-rec [number exp mod]
	(println number " " exp " " mod)
	(cond 
		(= 0 exp ) 1
		(even? exp) (rem (square (expmod-rec number (/ exp 2) mod)) mod)
		:else (rem (* number (expmod-rec number (dec exp) mod)) mod)))


(defn try-prime? [n]
	(if (< n 4)
		true
		(let [generated (+ 1 (rand-int (dec n)))]
			;(println "test on " generated)
			(= (expmod-rec generated n n) generated))))

(defn fast-prime? [n e]
	(cond 
		(= e 0) true
		(not (try-prime? n)) false
		:else (recur n (dec e))))

(defn fermat-test [n]
	(letfn [
		(fermat-test-iter [for-number]
			;(println for-number )
			(cond 
				(= for-number n) true
				(not (= for-number (expmod-rec for-number n n))) false
				:else (recur (inc for-number))))]
		(fermat-test-iter 2)))

(defn accumulate-rec [combine null-value with-term from-a to-next b]
	(if (> from-a b)
		null-value
		(combine 
			(with-term from-a) 
			(accumulate-rec combine null-value with-term (to-next from-a) to-next b))))

(defn accumulate-iter [combine null-value with-term from-a to-next b]
	(letfn [
		(iterate-on [current result]
			(if (> current b)
				result
				(recur 
					(to-next current) 
					(combine result (with-term current)))))]
		(iterate-on from-a null-value)))

(defn sum [with-term a for-next b]
	(accumulate-rec + 0 with-term a for-next b))


(defn sum-iter [with-term a for-next b]
	(accumulate-iter + 0 with-term a for-next b))

(defn mult [with-term from-a for-next to-b] 
	(accumulate-rec * 1 with-term from-a for-next to-b))

(defn multi-iter [with-term from-a for-next to-b]
	(accumulate-iter * 1 with-term from-a for-next to-b))

(defn id [a] a)

(defn sum-integers [from-a to-b]
	(sum-iter id from-a inc to-b))

(defn pi-estimate [b]
	(letfn [
		(term [a]
			(/ 1.0 (* a (+ a 2))))
		(for-next [a] (+ 4 a))]
		(* 8(sum term 1 for-next b))))

(defn integral [f a b dx]
	(*  dx
		(let [half-dx (/ dx 2)]
			(letfn [
				(term [x] 
					(f (+ x half-dx)))
				(for-next [x] (+ x dx))]
			(sum-iter term a for-next b)))))

(defn simpson [f a b n]
	(let [h (/ (- b a) n) th (* 2N h) limit (- b h)]
		(letfn [
			(term [x]
				(+ 
					(* 4N (f x ))
					(* 2N (f (+ x h)))))
			(for-next [x] (+ x th))]
			(*
				(/ h 3N)
				(+ (f a) (f b) (sum-iter term (+ a h) for-next limit))))))



(defn factorial [n]
	(multi-iter id 1 inc n))

(defn another-pi [n]
	(letfn [
		(with-term [x]
			(* 
				(/ x (dec x))
				(/ x (inc x))))
		(for-next [x] (inc (inc x)))]
	(* 2 (multi-iter with-term 2 for-next n))))


(defn good-enough? [a b]
	(letfn [
		(abs [n]
			(if (> n 0) 
				n
				(- n)))]
	(< (abs (- a b)) 0.0001)))

(defn search-zero-of [f a b]
	(println a b)
	(let [midpoint (/ (+ a b) 2)]
		(println "midpoint " midpoint)
		(if (good-enough? a b)
			midpoint
			(let [fm (f midpoint)]
				(cond 
					(> fm 0) (recur f a midpoint)
					(< fm 0) (recur f midpoint b)
					:else midpoint)))))

(defn zero-of [f a b]
	(let [fa (f a) fb (f b)]
		(cond 
			(and (< fa 0) (> fb 0))
				(search-zero-of f a b)
			(and (> fa 0) (< fb 0)) 
				(search-zero-of f b a)
			:else 
				(throw 
					(Exception. (str "Cannot find 0 between " a " and " b))))))

(defn average [a b] 
	(/ (+ a b) 2))

(defn average-damp [f]
	(fn [x] (average x (f x))))

(defn fixed-point [f first-guess]
	(letfn [
		(search [guess]
			(println guess)
			(let [next-guess (f guess)]
				(if (good-enough? guess next-guess)
					next-guess
					(recur next-guess))))]
		(search first-guess)))

(defn count-frac-one [n d depth]
	(letfn [
		(ratio [x] (/ (n x) (d x)))
		(count-frac-rec [step]
			(if (= 1 step)
				(ratio step)
				(/ (n step) (+ (d step) (count-frac-rec (dec step))))))]
	(count-frac-rec depth)))

(defn count-frac-one [n d depth]
	(letfn [
		(ratio [x] (/ (n x) (d x)))
		(count-frac-rec [step]
			(if (= 1 step)
				(ratio step)
				(/ (n step) (+ (d step) (count-frac-rec (dec step))))))]
	(count-frac-rec depth)))

(defn count-frac-two [n d depth]
	(letfn [
		(count-frac-iter [result step]
			(if (= 0 step)
				result
				(recur (/ (n step) (+ (d step) result)) (dec step))))]
	(count-frac-iter 0 depth)))

(defn exponential-denum [step]
	(let [remainder (rem step 3N) denum (int (/ step 3))]
		(println "remainder " remainder " denum " denum)
		(if (= 1 remainder)
			(* 2 (inc denum))
			1N)))

(defn tan-cf[x step]
	(letfn [
		(n [level] 
			(if (= 1 level)
				x 
				(- (* x x))))
		(d [level]
			(- (* 2 level) 1))]
	(count-frac-two n d step)))


(defn cube[x] (* x x x))

(defn deriv [g]
	(let [dx 0.00001]
		(fn[x] (/ (- (g (+ x dx)) (g x)) dx))))

(defn newton-transform[g]
	(fn [x] 
		(- x (/ (g x) ((deriv g) x)))))

(defn newton-method [g guess]
	(fixed-point (newton-transform g) guess))


(defn sqrt [x]
	(newton-method (fn [y] (- (square y) x)) 1.0))

(defn cubic [a b c] 
	(fn[x] (+ (cube x) (* a (square x)) (* b x) c)))

(defn zero-cubic [a b c]
	(newton-method (cubic a b c) 1.0))


(defn apply-twice [f]
	(fn[x] (f (f x))))

(defn compose [f g]
	(fn[x] (f (g x))))

(defn repeated [f n]
	(letfn [
		(apply-repetition [composed step]
			(if (= 0 step)
				composed
				(recur (compose f composed) (dec step))))]
		(apply-repetition f (dec n))))

(defn smooth [f]
	(let [dx 0.0001]
		(fn[x] 
			(/ (+ (f (+ x dx)) (f x) (f (- x dx)))
				3))))

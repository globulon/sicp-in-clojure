(ns sicp.part-two-intervals)

(defrecord Interval [lower-bound upper-bound])

(defn make-interval [lower upper]
	(Interval. lower upper))

(defn lower-bound [from-interval]
	(:lower-bound from-interval))

(defn upper-bound [from-interval]
	(:upper-bound from-interval))

(defn add-intervals [first-interval second-interval]
	(make-interval 
		(+ 
			(lower-bound first-interval) 
			(lower-bound second-interval))
		(+ 
			(upper-bound first-interval) 
			(upper-bound second-interval))))

(defn sub-intervals [first-interval second-interval]
	(add-intervals
		first-interval
		(make-interval 
			(- (upper-bound second-interval))
			(- (lower-bound second-interval)))))


(defn mul-intervals [first-interval second-interval]
	(let [
		p1 (* (lower-bound first-interval) (lower-bound second-interval))
		p2 (* (upper-bound first-interval) (upper-bound second-interval))
		p3 (* (upper-bound first-interval) (lower-bound second-interval))
		p4 (* (lower-bound first-interval) (upper-bound second-interval))]
		(make-interval 
			(min p1 p2 p3 p4)
			(max p1 p2 p3 p4))))

(defn- span-zero [interval]
	(>= 0 (* (upper-bound interval) (lower-bound interval))))

(defn width-interval [from-interval]
  (/ (-
       (upper-bound from-interval)
       (lower-bound from-interval))
    2.0))


(defn div-intervals [first-interval second-interval]
	(let [width (width-interval second-interval)]
		(if (span-zero second-interval) 
			(throw (Exception. "zero span interval"))
			(mul-intervals 
				first-interval
				(make-interval 
					(/ 1.0 (upper-bound second-interval))
					(/ 1.0 (lower-bound second-interval)))))))


(defn make-center-width [center-value width-value]
	(make-interval (- center-value width-value) (+ center-value width-value)))

(defn center [from-interval]
	(/ ( + 
			(upper-bound from-interval) 
			(lower-bound from-interval)) 
		2.0))

(defn make-percent [center-value percent-value]
	(let [width (/ (* center-value percent-value) 100.0)]
		(make-center-width center-value width)))

(defn percent [from-interval]
	(let [center-value (center from-interval)]
		(* 50.0
			(/ 
				(- 
					(upper-bound from-interval) 
					(lower-bound from-interval)) 
				center-value))))

(defn par1 [r1 r2]
	(div-intervals 
		(mul-intervals r1 r2)
		(add-intervals r1 r2)))

(defn par2 [r1 r2]
	(let [one (make-interval 1 1)]
		(div-intervals 
			one
			(add-intervals 
				(div-intervals one r1)
				(div-intervals one r2)))))

(defn a-test[]
	(let [interval (make-percent 10 1)]
		(println (div-intervals  interval interval)))
	(let [interval (make-percent 10 5)]
		(println (div-intervals  interval interval)))
	(let [interval (make-percent 10 10)]
		(println (div-intervals  interval interval)))
	(let [interval (make-percent 10 15)]
		(println (div-intervals  interval interval)))
	(let [interval (make-percent 10 20)]
		(println (div-intervals  interval interval)))
	(let [
		interval1 (make-percent 100 1)
		interval2 (make-percent 10 1)]
		(println (div-intervals  interval1 interval2)))
	(let [
		interval1 (make-percent 100 5)
		interval2 (make-percent 10 5)]
		(println (div-intervals  interval1 interval2)))
	(let [
		interval1 (make-percent 100 10)
		interval2 (make-percent 10 10)]
		(println (div-intervals  interval1 interval2)))
	(let [
		interval1 (make-percent 100 15)
		interval2 (make-percent 10 15)]
		(println (div-intervals  interval1 interval2))))

(defn b-test []
	(let [
		interval1 (make-percent 100 1)
		interval2 (make-percent 10 1)]
		(println (par1  interval1 interval2))
		(println (par2  interval1 interval2)))
	(let [
		interval1 (make-percent 100 5)
		interval2 (make-percent 10 5)]
		(println (par1  interval1 interval2))
		(println (par2  interval1 interval2)))
	(let [
		interval1 (make-percent 100 10)
		interval2 (make-percent 10 10)]
		(println (par1  interval1 interval2))
		(println (par2  interval1 interval2))))


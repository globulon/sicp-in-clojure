(ns sicp.part-two-segments)

(defn- square[x] (* x  x))

(defn make-point [x y]
	[x y])

(defn x-point [from-point]
	(first from-point))

(defn y-point [from-point]
	(second from-point))	

(defn make-segment [from-start to-end]
	[from-start to-end])

(defn start-segment [from-segment]
	(first from-segment))

(defn end-segment [from-segment]
	(second from-segment))	

(defn length-segment [segment]
	(let [
		start (start-segment segment)
		end (end-segment segment)
		dx (- (x-point start) (x-point end))
		dy (- (y-point start) (y-point end))]
	(Math/sqrt (+ (square dx) (square dy)))))

(defn make-rectangle [a b]
	(let [
		length-a (length-segment a)
		length-b (length-segment b)]
		(if (> length-a length-b) 
			{:width b :length a}
			{:width a :length b})))

(defn width-rectangle[from-rectangle]
	(:width from-rectangle))	

(defn length-rectangle[from-rectangle]
	(:length from-rectangle))	

(defn area-rectangle [rectangle]
	(* 
		(length-segment (width-rectangle rectangle)) 
		(length-segment (length-rectangle rectangle))))

(defn perimeter-rectangle [rectangle]
	(* 2 
		(+ 
			(length-segment (width-rectangle rectangle)) 
			(length-segment (length-rectangle rectangle)))))
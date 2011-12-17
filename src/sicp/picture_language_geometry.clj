(ns sicp.picture-language-geometry)

(defn make-vect [x y]
	[x y])

(defn xcor-vect [[x y]]
	x)

(defn ycor-vect [[x y]]
	y)

(defn op-vect [op vector-one vector-two]
	(make-vect
		(op (xcor-vect vector-one) (xcor-vect vector-two))
		(op (ycor-vect vector-one) (ycor-vect vector-two))))

(defn add-vect [vector-one vector-two]
	(op-vect + vector-one vector-two))

(defn sub-vect [vector-one vector-two]
	(op-vect - vector-one vector-two))

(defn scale-vect [alpha vect]
	(make-vect 
		(* alpha (xcor-vect vect))
		(* alpha (ycor-vect vect))))

(defn make-frame [origin edge-one edge-two]
	[origin edge-one edge-two])

(defn origin-frame [[origin _ _]]
	origin)

(defn edge-one [[_ edge-one _]]
	edge-one)	

(defn edge-two [[_ _ edge-two]]
	edge-two)	

(defn make-segment [start end]
	[start end])

(defn start-segment [[start _]]
	start)

(defn end-segment [[_ end]]
	end)

(defn map-coord-frame [frame]
	(fn [v]
		(add-vect 
			(origin-frame frame)
			(add-vect 
				(scale-vect (xcor-vect v) (edge-one frame))
				(scale-vect (ycor-vect v) (edge-two frame))))))
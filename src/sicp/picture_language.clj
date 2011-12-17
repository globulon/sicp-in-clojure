(ns sicp.picture-language
	(:use sicp.picture-language-geometry))

(defn transform-painter [painter origin corner-one corner-two]
	(fn [frame]
		(let [
			m (map-coord-frame frame)
			new-origin (m origin)]
			(painter 
				(make-frame 
					new-origin
					(sub-vect (m corner-one) new-origin)
					(sub-vect (m corner-two) new-origin))))))	

(defn beside [lft rght] 
	(let [
		left-panel (transform-painter 
									lft 
									(make-vect 0.0 0.0)
									(make-vect 0.5 0.0)
									(make-vect 0.0 1.0))
		right-panel (transform-painter 
									rght 
									(make-vect 0.5 0.0)
									(make-vect 1.0 0.0)
									(make-vect 0.0 1.0))]
		(fn [frame]
			(left-panel frame)
			(right-panel frame))))

(defn below [bottom top] 
	(let [
		top-panel 
			(tranform-painter 
				bottom 
				(make-vector 0.0 0.5)
				(make-vector 1.0 0.5)
				(make-vector 0.0 1.0))
		bottom-panel 
			(tranform-painter 
				bottom
				(make-vector 0.0 0.0)
				(make-vector 1.0 0.0)
				(make-vector 0.0 0.5))]
		(fn [frame]
			(bottom-panel frame)
			(top-panel frame))))

(defn flip-vert [painter] 
	(transform-painter 
		painter
		(make-vect 0.0 1.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)))

(defn flip-horiz [painter] 
	(transform-painter
		painter
		(make-vect 1.0 0.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)))

(defn rotate180 [painter] 
	(transform-painter
		painter
		(make-vect 1.0 1.0)
		(make-vect 0.0 1.0)
		(make-vect 1.0 0.0)))

(defn rotate270 [painter] 
	(transform-painter
		painter
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)))

(defn draw-line [start end])

(defn split [frame sub-frames]
	(fn [painter n]
		(if (= 0 n)
			painter 
			(let [smaller ((split frame sub-frames) painter (dec n))]
				(frame painter (sub-frames smaller smaller))))))

(defn square-of-four [tl tr bl br]
	(fn [painter]
		(let [
			top (beside (tl painter) (tr painter))
			bottom (beside (bl painter) (br painter))]
			(below bottom top))))

(defn flipped-pairs [painter]
	(let [
		combined (square-of-four identity flip-vert identity flip-vert)]
		(combined painter)))

(def right-split (split beside below))

(def up-split (split below beside))

(defn corner-split [painter n]
	(let [
		up-painter (up-split painter (dec n))
		right-painter (right-split painter (dec n))
		top-left (beside  up-painter up-painter)
		bottom-right (below right-painter right-painter)
		corner (corner-split painter (dec n))]
		(below 
			(painter bottom-right) 
			(beside top-left corner))))

(defn square-limit [painter n]
	(let [
		combined (square-of-four flip-horiz identity rotate180 flip-vert)]
		(combined (corner-split painter n))))

(defn segments->painter [segment-list]
	(fn [frame]
		(let [mapped (map-coord-frame frame)]
			(for [segment segment-list]
				(draw-line 
					(mapped (start-segment segment))
					(mapped (end-segment segment)))))))

(defn outline [frame]
	(let [
		bottom (make-segment (make-vect 0 0) (make-vect 1 0))
		rght (make-segment (make-vect 1 0) (make-vect 1 1))
		top (make-segment (make-vect 1 1) (make-vect 0 1))
		lft (make-segment (make-vect 0 1) (make-vect 0 0))]
		((segments->painter (list bottom rght top lft) frame))))

(defn draw-x [frame]
	(let [
		top-left-to-bottom-right (make-segment (make-vect 0 1) (make-vect 1 0))
		bottom-left-to-top-right (make-segment (make-vect 0 0) (make-vect 1 1))]
		((segments->painter 
			(list top-left-to-bottom-right bottom-left-to-top-right)) 
			frame)))

(defn diamond [frame] 
	(let [
		bottom (make-vect 0.5 0)
		rght (make-vect 1 0.5)
		top (make-vect 0.5 1)
		lft (make-vect 0 0.5)]
		((segments->painter
			(list 
				(make-segment bottom rght)
				(make-segment rght top)
				(make-segment top lft)
				(make-segment lft bottom))) 
			frame)))


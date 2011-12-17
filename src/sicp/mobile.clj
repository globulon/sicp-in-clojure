(ns sicp.mobile)

(defn make-branch[length structure]
	(list length structure))

(defn branch-length [branch]
	(first branch))

(defn branch-structure [branch]
	(second branch))
	
(defn make-mobile [l r]
	(list l r))	

(defn left-branch [mobile]
	(first mobile))	

(defn right-branch [mobile]
	(second mobile))	

(defn mobile-on? [branch] 
	(seq? (branch-structure branch)))

(defn weight-branch [rod]
	(branch-structure rod))

(defn total-weight-mobile [mobile]
	(letfn [
		(branch-weight [rod]
			(if (mobile-on? rod)
				(total-weight-mobile (branch-structure rod))
				(weight-branch rod)))
		]
	(+ (branch-weight (left-branch mobile))
		(branch-weight (right-branch mobile)))))

(defn torque-on[branch]
	(let [
		length (branch-length branch)
		structure (branch-structure branch)]
		(if (mobile-on? branch)
			(* length (total-weight-mobile structure))
			(* length structure))))

(defn mobiles-in [mobile]
	(map branch-structure (filter mobile-on? mobile)))

(defn balanced? [mobile]
	(let [
		left-branch-torque (torque-on (left-branch mobile))
		right-branch-torque (torque-on (right-branch mobile))
		balanced-torques (= left-branch-torque right-branch-torque)
		sub-mobiles (mobiles-in mobile)
		nb-sub-mobiles (count sub-mobiles)
		]
		(and 
			balanced-torques 
			(or (= 0 nb-sub-mobiles) (= nb-sub-mobiles (count (filter balanced? sub-mobiles)))))))

